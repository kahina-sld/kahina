package org.kahina.core.event;

import org.kahina.core.breakpoint.KahinaBreakpoint;

public class KahinaWarnEvent extends KahinaEvent
{

	private final KahinaBreakpoint breakpoint;
	private final int matchCount;

	public KahinaWarnEvent(KahinaBreakpoint breakpoint, int matchCount)
	{
		super(KahinaEventTypes.WARN);
		this.breakpoint = breakpoint;
		this.matchCount = matchCount;
	}
	
	public KahinaBreakpoint getBreakpoint()
	{
		return breakpoint;
	}
	
	public int getMatchCount()
	{
		return matchCount;
	}

}
