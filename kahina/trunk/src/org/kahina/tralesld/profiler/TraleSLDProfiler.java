package org.kahina.tralesld.profiler;

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
	protected void profileNode(KahinaTree tree, KahinaTree contentfulTree, int stepID, LogicProgrammingProfile profile)
	{
		TraleSLDStep step = KahinaRunner.retrieve(TraleSLDStep.class, stepID);
		profileNode(step, tree, contentfulTree, stepID, profile);
	}

	@Override
	protected void profileNode(ProfileEntry entry, LogicProgrammingStep step, KahinaTree tree, KahinaTree contentfulTree, int stepID, LogicProgrammingProfile profile)
	{
		super.profileNode(entry, step, tree, contentfulTree, stepID, profile);
		if (tree.getNodeStatus(stepID) == TraleSLDStepType.FINISHED)
		{
			profile.fail(entry);
		}
	}

}
