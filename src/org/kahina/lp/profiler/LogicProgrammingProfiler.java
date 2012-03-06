package org.kahina.lp.profiler;

import org.kahina.core.KahinaRunner;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.profiler.ProfileEntry;
import org.kahina.core.util.Mapper;
import org.kahina.lp.LogicProgrammingInstance;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.lp.LogicProgrammingStepType;
import org.kahina.lp.bridge.LogicProgrammingBridgeEvent;
import org.kahina.lp.bridge.LogicProgrammingBridgeEventType;

public class LogicProgrammingProfiler implements KahinaListener
{

	private static final boolean VERBOSE = false;

	private final Mapper<String, ProfileEntry> mapper;

	private final LogicProgrammingProfile profile;
	
	protected final LogicProgrammingInstance<?,?,?> kahina;

	public LogicProgrammingProfiler(LogicProgrammingInstance<?,?,?> kahina, Mapper<String, ProfileEntry> mapper, LogicProgrammingProfile profile)
	{
		this.mapper = mapper;
		this.profile = profile;
		this.kahina = kahina;
		// Currently this does not make a lot of sense since the full profile
		// could easily be computed on demand from the control flow tree just
		// like subtree profiles. This will change once the profiler keeps track
		// of the time spent in individual steps - then we will need to record
		// extra information that is not part of the control flow tree.
		kahina.getControl().registerListener("logic programming bridge", this);
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
		} else if (eventType == LogicProgrammingBridgeEventType.STEP_EXCEPTION)
		{
			exception(event.getID());
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
	
	protected void exception(int id)
	{
		profile.exception(getProfileEntryForStepID(id));
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
		return mapper.map(kahina.getState().retrieve(LogicProgrammingStep.class, stepID).getGoalDesc());
	}

	public LogicProgrammingProfile getProfile()
	{
		return profile;
	}

	/**
	 * Creates a profile for the subtree rooted in a given node.
	 * 
	 * @param tree
	 *            The tree object used for determining the descendants of the
	 *            node given as subtree node. This can either be the primary or
	 *            the secondary tree.
	 * @param contentfulTree
	 *            The tree object used for determinining the status of nodes.
	 *            This usually needs to be the primary tree, as in the current
	 *            architecture, node statuses and labels are only stored in the
	 *            primary tree.
	 * @param subtreeRootID
	 *            The (internal) ID of the root of the subtree the caller wants
	 *            to profile.
	 * @return A profile for the specified subtree.
	 */
	public LogicProgrammingProfile profileSubtree(KahinaTree tree, KahinaTree contentfulTree, int subtreeRootID)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".profileSubtree(" + tree + ", " + subtreeRootID + ")");
		}
		LogicProgrammingProfile result = new LogicProgrammingProfile();
		profileSubtree(tree, contentfulTree, subtreeRootID, result);
		return result;
	}

	private void profileSubtree(KahinaTree tree, KahinaTree contentfulTree, int stepID, LogicProgrammingProfile profile)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".profileSubtree(" + tree + ", " + stepID + ", " + profile + ")");
		}
		profileNode(tree, contentfulTree, stepID, profile);
		for (int childID : tree.getChildren(stepID))
		{
			profileSubtree(tree, contentfulTree, childID, profile);
		}
	}

	protected void profileNode(KahinaTree tree, KahinaTree contentfulTree, int stepID, LogicProgrammingProfile profile)
	{
		LogicProgrammingStep step = kahina.getState().retrieve(LogicProgrammingStep.class, stepID);
		profileNode(step, tree, contentfulTree, stepID, profile);
	}

	protected void profileNode(LogicProgrammingStep step, KahinaTree tree, KahinaTree contentfulTree, int stepID, LogicProgrammingProfile profile)
	{
		ProfileEntry entry = mapper.map(step.getGoalDesc());
		profileNode(entry, step, tree, contentfulTree, stepID, profile);
	}

	protected void profileNode(ProfileEntry entry, LogicProgrammingStep step, KahinaTree tree, KahinaTree contentfulTree, int stepID, LogicProgrammingProfile profile)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".profileNode(" + entry + ", " + step + ", " + tree + ", " + stepID + ", " + profile + ")");
		}
		if (step.isRedone())
		{
			profile.redo(entry);
		} 
		else
		{
			profile.call(entry);
		}

		int status = contentfulTree.getNodeStatus(stepID);
		if (VERBOSE)
		{
			System.err.println("Status: " + status);
		}

		if (status == LogicProgrammingStepType.DET_EXIT || status == LogicProgrammingStepType.EXIT)
		{
			if (VERBOSE)
			{
				System.err.println("Exited.");
			}
			profile.exit(entry);
		} 
		else if (status == LogicProgrammingStepType.FAIL)
		{
			if (VERBOSE)
			{
				System.err.println("Failed.");
			}
			profile.fail(entry);
		}
	}

}
