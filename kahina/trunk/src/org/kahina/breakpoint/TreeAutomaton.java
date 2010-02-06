package org.kahina.breakpoint;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import org.kahina.control.KahinaController;
import org.kahina.control.event.KahinaTreeMatchEvent;
import org.kahina.data.tree.KahinaTree;

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
    
    /**
     * annotates a tree node with all possible labels according to the rules
     * recursively reannotates parents if it triggers a change in annotation, 
     * @param nodeID - the node to be (re)annotated by the automaton
     */
    public void process(int nodeID)
    {
        //TODO: perhaps introduce a way of detecting the loss of annotations as well
        boolean annotationChange = false;
        for (TreeAutomatonRule rule : rules)
        {
            if (rule.ruleApplies(this, nodeID))
            {
                if (annotate(nodeID, rule.getAssignedLabel()))
                {
                    annotationChange = true;
                }
            }
        }
        if (annotationChange)
        {
            int parentID = tree.getParent(nodeID, 0);
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
        Set<Integer> ann = annotations.get(nodeID);
        if (ann == null)
        {
            ann = new HashSet<Integer>();
            annotations.put(nodeID, ann);
        }
        //if this annotation
        if (ann.add(stateID))
        {
            if (acceptingStates.contains(stateID))
            {
                announcePatternMatch(nodeID);
            }
            return true;
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
        for (int childID : tree.getChildren(nodeID, 0))
        {
            childAnn.addAll(getAnnotations(childID));
        }
        return childAnn;
    }
    
    private void announcePatternMatch(int nodeID)
    {
        if (ctrl == null)
        {
            System.err.println("WARNING! Tree pattern match, but no controller is listening!");
        }
        else
        {
            ctrl.processEvent(new KahinaTreeMatchEvent(this, nodeID));
        }
    }
    
    public String toString()
    {
        String str = "TreeAutomaton";
        str += "   states: " + states + "\n";
        str += "   accepting states: " + acceptingStates + "\n";
        str += "   rules:\n";
        for (TreeAutomatonRule rule : rules)
        {
            str += "      " + rule.toString();
        }
        return str;
    }  
}
