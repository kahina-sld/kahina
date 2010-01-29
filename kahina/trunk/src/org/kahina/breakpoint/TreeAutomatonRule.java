package org.kahina.breakpoint;

import java.util.HashSet;
import java.util.Set;

/**
 * A rule states that a node with whose children's annotations include the requiredChildAnnotations
 * and whose node info matches the (freely specifiable) pattern
 * may be annotated by the tree automaton with the assignedLabel.
 * 
 * @author johannes
 *
 */

public class TreeAutomatonRule
{
    //no order can be imposed on the required child annotations by this type of rule
    Set<Integer> requiredChildAnnotations;
    //second part of the LHS: allow all kinds of conditions on the node to be annotated
    TreeNodePattern pattern;
    //-1 means: no annotation, won't be stored by the tree automaton
    int assignedLabel;
    
    //the default rule: match anything, annotate nothing
    public TreeAutomatonRule()
    {
        requiredChildAnnotations = new HashSet<Integer>();
        pattern = new TreeNodePattern();
        assignedLabel = -1;
    }
    
    public int getAssignedLabel()
    {
        return assignedLabel;
    }
    
    public boolean ruleApplies(TreeAutomaton aut, int nodeID)
    {
        if (pattern.matches(aut.tree, nodeID))
        {
            if (aut.getChildAnnotations(nodeID).containsAll(requiredChildAnnotations))
            {
                return true;
            }
        }
        return false;
    }
}
