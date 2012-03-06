package org.kahina.tralesld.profiler;

import org.kahina.core.KahinaRunner;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.profiler.ProfileEntry;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.lp.profiler.LogicProgrammingProfile;
import org.kahina.lp.profiler.LogicProgrammingProfiler;
import org.kahina.tralesld.TraleSLDInstance;
import org.kahina.tralesld.TraleSLDStep;
import org.kahina.tralesld.TraleSLDStepType;
import org.kahina.tralesld.bridge.TraleSLDBridgeEvent;
import org.kahina.tralesld.bridge.TraleSLDBridgeEventType;

public class TraleSLDProfiler extends LogicProgrammingProfiler
{

	public TraleSLDProfiler(TraleSLDInstance kahina, LogicProgrammingProfile profile)
	{
		super(kahina, new TraleSLDProfileEntryMapper(), profile);
		kahina.getControl().registerListener("traleSLD bridge", this);
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
		TraleSLDStep step = kahina.getState().retrieve(TraleSLDStep.class, stepID);
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
