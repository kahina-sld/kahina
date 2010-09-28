package org.kahina.lp.profiler;

import org.kahina.core.KahinaRunner;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.profiler.ProfileEntry;
import org.kahina.core.util.Mapper;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.lp.LogicProgrammingStepType;
import org.kahina.lp.event.LogicProgrammingBridgeEvent;
import org.kahina.lp.event.LogicProgrammingBridgeEventType;

public class LogicProgrammingProfiler implements KahinaListener
{
	
	private static final boolean VERBOSE = true;
	
	private final Mapper<String, ProfileEntry> mapper;
	
	private final LogicProgrammingProfile profile;
	
	public LogicProgrammingProfiler(Mapper<String, ProfileEntry> mapper, LogicProgrammingProfile profile)
	{
		this.mapper = mapper;
		this.profile = profile;
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

	public LogicProgrammingProfile profileSubtree(KahinaTree tree, int subtreeRootID)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".profileSubtree(" + tree + ", " + subtreeRootID + ")");
		}
		LogicProgrammingProfile result = new LogicProgrammingProfile();
		profileSubtree(tree, subtreeRootID, result);
		return result;
	}

	private void profileSubtree(KahinaTree tree, int stepID, LogicProgrammingProfile profile)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".profileSubtree(" + tree + ", " + stepID + ", " + profile + ")");
		}
		profileNode(tree, stepID, profile);
		for (int childID : tree.getChildren(stepID))
		{
			profileSubtree(tree, childID, profile);
		}
	}

	protected void profileNode(KahinaTree tree, int stepID, LogicProgrammingProfile profile)
	{
		LogicProgrammingStep step = KahinaRunner.retrieve(LogicProgrammingStep.class, stepID);
		profileNode(step, tree, stepID, profile);
	}

	protected void profileNode(LogicProgrammingStep step, KahinaTree tree, int stepID, LogicProgrammingProfile profile)
	{
		ProfileEntry entry = mapper.map(step.getGoalDesc());
		profileNode(entry, step, tree, stepID, profile);
	}

	protected void profileNode(ProfileEntry entry, LogicProgrammingStep step, KahinaTree tree, int stepID, LogicProgrammingProfile profile)
	{
		if (step.isRedone())
		{
			profile.redo(entry);
		} else
		{
			profile.call(entry);
		}
		int status = tree.getNodeStatus(stepID);
		if (status == LogicProgrammingStepType.DET_EXIT || status == LogicProgrammingStepType.EXIT)
		{
			profile.exit(entry);
		} else if (status == LogicProgrammingStepType.FAIL)
		{
			profile.fail(entry);
		}
	}

}
