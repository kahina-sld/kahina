package org.kahina.core.data.agent.patterns;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import org.kahina.core.KahinaInstance;
import org.kahina.core.behavior.KahinaTreeBehavior;
import org.kahina.core.control.KahinaController;
import org.kahina.core.data.agent.KahinaBreakpoint;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.data.tree.KahinaUnlayeredMemTree;

/**
 * Implements a tree automaton as used for tree pattern matching by Kahina's breakpoint system.
 * <p>
 * This special kind of bottom-up tree automaton operates on a tree it is monitoring.
 * It does not only annotate a static tree to determine whether it matches the encoded pattern,
 * but is also able to adapt to changes to the tree without recalculating everything,
 * if the component controlling changes in the tree structure (usually a subclass of {@link KahinaTreeBehavior}) announces the changes.
 * <p>
 * A tree automaton is usually compiled from a {@link KahinaBreakpoint}, which contains
 * If the associated breakpoint is active, the automaton will dispatch a {@link KahinaTreeMatchEvent} to its <code>KahinaController</code> as soon as the encoded pattern is found in the tree it monitors.
 * 
 * @author jd
 *
 */

public class TreeAutomaton
{
    static boolean VERBOSE = false;
    
    /*logic of the tree automaton*/
    //also the possible annotations (no negative integers!)
    Set<Integer> states;
    //which state annotation corresponds to a pattern match
    Set<Integer> acceptingStates;
    //encode information on how to annotate trees
    Set<TreeAutomatonRule> rules;
    
    /*working data*/ 
    //the tree that is currently being monitored
    KahinaTree tree;
    //a map from node IDs to state IDs they are annotated with
    HashMap<Integer,Set<Integer>> annotations;
    
    //reports to this instance if the encoded pattern was found in the tree
    KahinaInstance<?,?,?,?> kahina;
    //link to the breakpoint this automaton implements; contains more contextual info
    KahinaBreakpoint bp;
    
    //flag do determine whether the automaton reports matches directly above new nodes twice
    //TODO: generalize for patterns of depth > 2
    boolean constellationMatch = false;
    
    /**
     * Class constructor specifying the breakpoint to be associated with the new automaton.
     * The first thing to be called by a <code>KahinaBreakpoint</code> when compiled.
     * @param bp the breakpoint object to be associated with the new automaton
     */
    public TreeAutomaton(KahinaBreakpoint bp)
    {
        states = new HashSet<Integer>();
        acceptingStates = new HashSet<Integer>();
        rules = new HashSet<TreeAutomatonRule>();
        
        this.tree = new KahinaUnlayeredMemTree();
        this.annotations =  new HashMap<Integer,Set<Integer>>();
        this.bp = bp;    
    }
    
    /**
     * Sets or changes the tree this automaton is monitoring.
     * The automaton will silently recompute its annotations in bottom-up manner.
     * @param tree the tree model to be monitored by this automaton
     */
    public void setTree(KahinaTree tree)
    {
        annotations =  new HashMap<Integer,Set<Integer>>();
        this.tree = tree;
        boolean wasActive = bp.isActive();
        this.bp.deactivate();
        for (int leafID : tree.getLeaves())
        {
            process(leafID);
        }
        if (wasActive) this.bp.activate();
    }
    
    /**
     * Determine whether the automaton reports matches directly above new nodes twice.
     * Default value: <code>false</code>, which is suitable in most contexts.
     * @param constellationMatch <code>true</code> for extra reports, <code>false</code> to suppress them
     */
    public void setConstellationMatch(boolean constellationMatch)
    {
        this.constellationMatch = constellationMatch;
    }
    
    public KahinaInstance<?,?,?,?> getKahina()
    {
        return kahina;
    }

    /**
     * Sets the Kahina instance this automaton is to inform about matches.
     * Default is <code>null</code>, an instance must be determined for the breakpoint system to work.
     * @param kahina the Kahina instance this automaton is to inform about matches
     */
    public void setKahina(KahinaInstance<?,?,?,?> kahina)
    {
        this.kahina = kahina;
    }

    /**
     * Annotates a tree node with all possible labels according to the rules and
     * recursively reannotates parents if it triggers a change in annotation. 
     * @param nodeID the node to be (re)annotated by the automaton
     */
    public void process(int nodeID)
    {
        if (VERBOSE) System.err.println("Skip Point automaton processing node " + nodeID + " " + annotations.get(nodeID));
        //TODO: perhaps introduce a way of detecting the loss of annotations as well
        boolean annotationChange = false;
        for (TreeAutomatonRule rule : rules)
        {
            if (VERBOSE) System.err.println("  Trying to apply rule " + rule);
            if (rule.ruleApplies(this, nodeID))
            {
                if (VERBOSE) System.err.println("  Success!");
                if (annotate(nodeID, rule.getAssignedLabel()))
                {
                    annotationChange = true;
                }
            }
            else
            {
                if (VERBOSE) System.err.println("  Failure!");
            }
        }
        if (annotationChange)
        {
            int parentID = tree.getParent(nodeID);
            if (parentID != -1)
            {
                process(parentID);
            }
        }
    }
    
    /**
     * Annotates a single node in the tree with some state ID and checks whether this resulted in a change.
     * @param nodeID the node to be annotated
     * @param stateID the state ID the node is to be annotated with
     * @return false if this annotation existed before, true if it was new
     */
    public boolean annotate(int nodeID, int stateID)
    {
        if (VERBOSE) System.err.println("Skip Point automaton processing node " + nodeID);
        Set<Integer> ann = annotations.get(nodeID);
        if (ann == null)
        {
            ann = new HashSet<Integer>();
            annotations.put(nodeID, ann);
        }
        if (ann.add(stateID))
        {
            //a match is detected if annotation with an accepting state occurs
            if (acceptingStates.contains(stateID))
            {
                announcePatternMatch(nodeID);
            }
            return true;
        }
        if (constellationMatch)
        {
            if (acceptingStates.contains(stateID))
            {
                announcePatternMatch(nodeID);
            }
        }
        return false;
    }
    
    /**
     * Retrieves the current annotations for a single tree node.
     * @param nodeID addresses the node whose annotations we want to get
     * @return a set of stateIDs representing the annotations for the node
     */
    public Set<Integer> getAnnotations(int nodeID)
    {
        Set<Integer> ann = annotations.get(nodeID);
        if (ann == null)
        {
            ann = new HashSet<Integer>();
        }
        return ann;
    }
    
    /**
     * Retrieves the current annotations for all the children of a single tree node.
     * @param nodeID addresses the node whose children's annotations we want to get
     * @return a set of stateIDs containing all the annotations for the children of the node
     */
    public Set<Integer> getChildAnnotations(int nodeID)
    {
        Set<Integer> childAnn = new HashSet<Integer>();
        for (int childID : tree.getChildren(nodeID))
        {
            childAnn.addAll(getAnnotations(childID));
        }
        return childAnn;
    }
    
    public int nextStateNumber()
    {
    	return states.size();
    }
    
    public void addState(int state)
    {
    	states.add(state);
    }
    
    public void addAcceptingState(int state)
    {
    	acceptingStates.add(state);
    }
    
    public void addRule(TreeAutomatonRule rule)
    {
    	rules.add(rule);
    }
    
    //the mechanism for informing the KahinaController about matches
    private void announcePatternMatch(int nodeID)
    {
        if (VERBOSE) System.err.println("Skip Point automaton matched at node " + nodeID);
        if (VERBOSE && kahina == null)
        {
            System.err.println("WARNING! Tree pattern match, but no Kahina instance is listening!");
        }
        else
        {
            if (bp.isActive())
            {
                if (VERBOSE) System.err.println("Skip Point fired TreeMatchEvent at node " + nodeID);
                kahina.dispatchEvent(new KahinaTreeMatchEvent(bp, nodeID));
            }
        }
    }
    
    @Override
	public String toString()
    {
        String str = "TreeAutomaton\n";
        str += "   states: " + states + "\n";
        str += "   accepting states: " + acceptingStates + "\n";
        str += "   rules:\n";
        for (TreeAutomatonRule rule : rules)
        {
            str += "      " + rule.toString() + "\n";
        }
        return str;
    }  
}
