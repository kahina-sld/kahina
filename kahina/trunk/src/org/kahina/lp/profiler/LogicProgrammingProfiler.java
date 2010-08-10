package org.kahina.lp.profiler;

import org.kahina.core.KahinaRunner;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.profiler.ProfileEntry;
import org.kahina.core.util.Mapper;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.lp.event.LogicProgrammingBridgeEvent;
import org.kahina.lp.event.LogicProgrammingBridgeEventType;

public class LogicProgrammingProfiler implements KahinaListener
{
	
	private final Mapper<String, ProfileEntry> mapper;
	
	private final LogicProgrammingProfile profile = new LogicProgrammingProfile();
	
	public LogicProgrammingProfiler(Mapper<String, ProfileEntry> mapper)
	{
		this.mapper = mapper;
		KahinaRunner.getControl().registerListener("logic programming bridge", this);
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
		int eventType = event.getEventType();
		if (eventType == LogicProgrammingBridgeEventType.STEP_CALL)
		{
			call(event.getID());
		} else if (eventType == LogicProgrammingBridgeEventType.STEP_FAIL)
		{
			fail(event.getID());
		} else if (eventType == LogicProgrammingBridgeEventType.STEP_DET_EXIT)
		{
			exit(event.getID());
		} else if (eventType == LogicProgrammingBridgeEventType.STEP_NONDET_EXIT)
		{
			exit(event.getID());
		} else if (eventType == LogicProgrammingBridgeEventType.STEP_REDO)
		{
			redo(event.getID());
		}
	}
	
	protected void call(int id)
	{
		profile.call(getProfileEntryForStepID(id));
	}
	
	protected void fail(int id)
	{
		profile.fail(getProfileEntryForStepID(id));
	}
	
	protected void exit(int id)
	{
		profile.exit(getProfileEntryForStepID(id));
	}
	
	protected void redo(int id)
	{
		profile.redo(getProfileEntryForStepID(id));
	}

	protected ProfileEntry getProfileEntryForStepID(int stepID)
	{
		return mapper.map(KahinaRunner.retrieve(LogicProgrammingStep.class, stepID).getGoalDesc());
	}
	
	public LogicProgrammingProfile getProfile()
	{
		return profile;
	}

}
