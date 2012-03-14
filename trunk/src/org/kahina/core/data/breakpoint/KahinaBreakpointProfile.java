package org.kahina.core.data.breakpoint;

import java.util.ArrayList;
import java.util.List;

import org.kahina.core.data.KahinaObject;

public class KahinaBreakpointProfile extends KahinaObject
{
	List<KahinaBreakpoint> treeBreakpoints;
	
	public KahinaBreakpointProfile()
	{
		treeBreakpoints = new ArrayList<KahinaBreakpoint>();
	}
}
