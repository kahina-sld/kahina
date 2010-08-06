package org.kahina.lp.profiler;

import org.kahina.core.control.KahinaListener;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.profiler.ProfileEntry;
import org.kahina.core.util.Mapper;
import org.kahina.lp.event.LogicProgrammingBridgeEvent;

public class LogicProgrammingProfiler implements KahinaListener
{
	
	private final Mapper<String, ProfileEntry> mapper;
	
	public LogicProgrammingProfiler(Mapper<String, ProfileEntry> mapper)
	{
		this.mapper = mapper;
	}

	@Override
	public void processEvent(KahinaEvent event)
	{
		if (event instanceof LogicProgrammingBridgeEvent)
		{
			processLogicProgrammingBridgeEvent((LogicProgrammingBridgeEvent) event);
		}
	}

	private void processLogicProgrammingBridgeEvent(LogicProgrammingBridgeEvent event)
	{
		// TODO Funny, there is no CALL event type yet. Need to change that first.
	}

}
