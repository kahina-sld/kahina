package org.kahina.core.control;

import org.kahina.core.control.patterns.TreeNodePattern;
import org.kahina.core.control.patterns.TreePattern;
import org.kahina.core.control.patterns.TreePatternNode;

/**
 * A collection of convenience methods for creating simple breakpoints.
 * @author ke
 *
 */
public class KahinaBreakpointFactory
{	
	/**
	 * Creates a breakpoint for nodes whose labels match a given regular expression.
	 * @param name a user-readable name for this breakpoint
	 * @param pattern the regular expression that node labels are to match
	 * @param type one of the constant values in {@link KahinaBreakpointType}
	 * @return a new breakpoint matching any tree node whose label matches the pattern
	 */
	public static KahinaBreakpoint createMatchingLabelBreakpoint(String name, String pattern, int type)
	{
		TreePattern pat = new TreePattern();
        TreePatternNode rootNode = new TreePatternNode();
        TreeNodePattern rootPattern = new TreeNodePattern(TreeNodePattern.CAPTION, TreeNodePattern.MATCHING, pattern);    
        rootNode.setPattern(rootPattern);
        pat.setRoot(rootNode);
        KahinaBreakpoint bp = new KahinaBreakpoint(type);
        bp.setName(name);
        bp.setPattern(pat);
        return bp;
	}
}
