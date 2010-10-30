package org.kahina.core.breakpoint;

/**
 * A collection of convenience methods for creating simple breakpoints.
 * @author ke
 *
 */
public class KahinaBreakpointFactory
{
	
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
