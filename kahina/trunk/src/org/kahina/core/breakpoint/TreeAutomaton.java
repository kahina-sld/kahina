package org.kahina.core.breakpoint;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.data.tree.KahinaUnlayeredMemTree;
import org.kahina.core.event.KahinaTreeMatchEvent;

/**
 * This special kind of bottom-up tree automaton operates on a tree it is monitoring.
 * It does not only annotate a static tree to determine whether it matches the encoded pattern,
 * but is also able to adapt to changes to the tree without recalculating everything,
 * if the component controlling changes in the tree structure (usually TreeBehavior) announces the changes.
 * 
 * Used by Kahinas breakpoint mechanism as the core device for defining breakpoints.
 * Will dispatch a KahinaTreeMatchEvent to a KahinaController as soon as the encoded pattern is found in the tree it monitors.
 * 
 * @author johannes
 *
 */

public class TreeAutomaton
{
    public static boolean VERBOSE = false;
    
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
    
    //reports to this controller if the encoded pattern was found in the tree
    KahinaController ctrl;
    //link to the breakpoint this automaton implements; contains more contextual info
    KahinaBreakpoint bp;
    
    //flag do determine whether the automaton reports matches directly over new nodes twice
    //TODO: generalize for patterns of depth > 2
    boolean constellationMatch = false;
    
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
     * set or change the tree this breakpoint is monitoring
     * the automaton will silently recompute its annotations in bottom-up manner
     * @param tree - the tree model this breakpoint is monitoring
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
    
    public void setConstellationMatch(boolean constellationMatch)
    {
        this.constellationMatch = constellationMatch;
    }
    
    public KahinaController getController()
    {
        return ctrl;
    }

    public void setController(KahinaController ctrl)
    {
        this.ctrl = ctrl;
    }

    /**
     * annotates a tree node with all possible labels according to the rules
     * recursively reannotates parents if it triggers a change in annotation, 
     * @param nodeID - the node to be (re)annotated by the automaton
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
     * annotates a single node in the tree
     * @param nodeID - which node is to be annotated
     * @param stateID - the annotation for the node
     * @return whether this annotation changed something
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
     * retrieves the current annotations for a single tree node
     * @param nodeID - addresses the node whose annotations we want to get
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
     * retrieves the current annotations for all the children a single tree node
     * @param nodeID - addresses the node whose children's annotations we want to get
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
    
    private void announcePatternMatch(int nodeID)
    {
        if (VERBOSE) System.err.println("Skip Point automaton matched at node " + nodeID);
        if (VERBOSE && ctrl == null)
        {
            System.err.println("WARNING! Tree pattern match, but no controller is listening!");
        }
        else
        {
            if (bp.isActive())
            {
                if (VERBOSE) System.err.println("Skip Point fired TreeMatchEvent at node " + nodeID);
                ctrl.processEvent(new KahinaTreeMatchEvent(bp, nodeID));
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
