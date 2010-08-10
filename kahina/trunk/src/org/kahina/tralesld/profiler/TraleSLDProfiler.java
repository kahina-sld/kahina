package org.kahina.tralesld.profiler;

import java.util.Set;

import org.kahina.core.KahinaRunner;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.profiler.ProfileEntry;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.lp.profiler.LogicProgrammingProfile;
import org.kahina.lp.profiler.LogicProgrammingProfiler;
import org.kahina.tralesld.TraleSLDStep;
import org.kahina.tralesld.TraleSLDStepType;
import org.kahina.tralesld.control.event.TraleSLDBridgeEvent;
import org.kahina.tralesld.control.event.TraleSLDBridgeEventType;

public class TraleSLDProfiler extends LogicProgrammingProfiler
{

	public TraleSLDProfiler(LogicProgrammingProfile profile)
	{
		super(new TraleSLDProfileEntryMapper(), profile);
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
	
	@Override
	protected void profileNode(KahinaTree tree, int stepID, LogicProgrammingProfile profile, Set<Integer> externalIDs)
	{
		TraleSLDStep step = KahinaRunner.retrieve(TraleSLDStep.class, stepID);
		profileNode(step, tree, stepID, profile, externalIDs);
	}
	
	@Override
	protected void profileNode(ProfileEntry entry, LogicProgrammingStep step, KahinaTree tree, int stepID, LogicProgrammingProfile profile, Set<Integer> externalIDs)
	{
		super.profileNode(step, tree, stepID, profile, externalIDs);
		if (tree.getNodeStatus(stepID) == TraleSLDStepType.FINISHED)
		{
			profile.fail(entry);
		}
	}

}
