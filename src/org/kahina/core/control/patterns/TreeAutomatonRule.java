package org.kahina.core.control.patterns;

import java.util.HashSet;
import java.util.Set;


/**
 * Encodes a rule within a {@link TreeAutomaton}.
 * <p>
 * A rule states that the automaton is to annotate with some <code>assignedLabel</code> every node where
 * <ol>
 * 	<li>the children's annotations include a given set of integers, the <code>requiredChildAnnotations</code></li>
 *  <li>the node matches a given tree node pattern, the <code>pattern</code></li>
 * </ol>
 *
 * @author jd
 *
 */

public class TreeAutomatonRule
{
    public static boolean verbose = false;
    
    //first condition; no order can be imposed on the required child annotations by this type of rule
    private Set<Integer> requiredChildAnnotations;
    //second condition: allow all kinds of conditions on the node to be annotated
    private TreeNodePattern pattern;
    //-1 means: no annotation, won't be stored by the tree automaton
    private int assignedLabel;
    
    /**
     * Class contructor. The default rule matches anything and annotates nothing.
     */
    public TreeAutomatonRule()
    {
        setRequiredChildAnnotations(new HashSet<Integer>());
        setPattern(new TreeNodePattern());
        setAssignedLabel(-1);
    }
    /**
     * Check whether this rule applies to a tree node in an automaton at the current state.
     * @param aut the automaton that is to be checked (monitoring some tree structure)
     * @param nodeID the tree node to be checked
     * @return true if this rule applies, false otherwise
     */
    public boolean ruleApplies(TreeAutomaton aut, int nodeID)
    {
        if (getPattern().matches(aut.tree, nodeID))
        {
            if (verbose) System.err.println("    pattern.matches(" + nodeID + ")");
            if (verbose) System.err.println("    child annotations: " + aut.getChildAnnotations(nodeID));
            if (aut.getChildAnnotations(nodeID).containsAll(getRequiredChildAnnotations()))
            {
                return true;
            }
        }
        return false;
    }
    
    @Override
	public String toString()
    {
        String str = getAssignedLabel() + " <- ";
        str += getRequiredChildAnnotations() + ", ";
        str += getPattern().toString();
        return str;
    }

    /**
     * Changes the set of child annotations required for the rule to apply.
     * @param requiredChildAnnotations a set of integers representing required child annotations
     */
	public void setRequiredChildAnnotations(Set<Integer> requiredChildAnnotations) 
	{
		this.requiredChildAnnotations = requiredChildAnnotations;
	}

	/**
	 * Gets the set of child annotations required for the rule to apply.
	 * @return a set of integers representing the required child annotations
	 */
	public Set<Integer> getRequiredChildAnnotations() 
	{
		return requiredChildAnnotations;
	}

	/**
	 * Changes the tree node pattern required for the rule to apply.
	 * @param pattern a tree node pattern defining when the rule is to apply
	 */
	public void setPattern(TreeNodePattern pattern) 
	{
		this.pattern = pattern;
	}

	/**
	 * Gets the tree node pattern required for the rule to apply.
	 * @return pattern a tree node pattern defining when the rule applies
	 */
	public TreeNodePattern getPattern() 
	{
		return pattern;
	}

	/**
	 * Sets the annotation to be added to nodes to which the rule applies.
	 * @param assignedLabel a stateID a matching node is to be annotated with
	 */
	public void setAssignedLabel(int assignedLabel) 
	{
		this.assignedLabel = assignedLabel;
	}

	/**
	 * Gets the annotation that is added to nodes to which the rule applies.
	 * @return the stateID that matching nodes are annotated with
	 */
	public int getAssignedLabel() 
	{
		return assignedLabel;
	}
}
