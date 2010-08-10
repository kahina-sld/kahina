package org.kahina.lp.profiler;

import org.kahina.core.KahinaRunner;
import org.kahina.core.event.KahinaEvent;
import org.kahina.tralesld.control.event.TraleSLDBridgeEvent;
import org.kahina.tralesld.control.event.TraleSLDBridgeEventType;
import org.kahina.tralesld.profiler.TraleSLDProfileEntryMapper;

public class TraleSLDProfiler extends LogicProgrammingProfiler
{

	public TraleSLDProfiler()
	{
		super(new TraleSLDProfileEntryMapper());
		KahinaRunner.getControl().registerListener("traleSLD bridge", this);
	}

	@Override
	public void processEvent(KahinaEvent event)
	{
		super.processEvent(event);
		if (event instanceof TraleSLDBridgeEvent)
		{
			processTraleSLDBridgeEvent((TraleSLDBridgeEvent) event);
		}
	}

	private void processTraleSLDBridgeEvent(TraleSLDBridgeEvent event)
	{
		if (event.getEventType() == TraleSLDBridgeEventType.STEP_FINISHED)
		{
			fail(event.getInternalID());
		}
	}

}
