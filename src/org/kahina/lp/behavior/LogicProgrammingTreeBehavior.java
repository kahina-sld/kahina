package org.kahina.lp.behavior;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaRunner;
import org.kahina.core.behavior.KahinaTreeBehavior;
import org.kahina.core.bridge.KahinaStepDescriptionEvent;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.control.KahinaSystemEvent;
import org.kahina.core.control.patterns.TreeAutomaton;
import org.kahina.core.data.breakpoint.KahinaBreakpoint;
import org.kahina.core.data.breakpoint.KahinaBreakpointType;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.data.tree.KahinaTreeEvent;
import org.kahina.core.data.tree.KahinaTreeEventType;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.lp.LogicProgrammingStepType;
import org.kahina.lp.bridge.LogicProgrammingBridgeEvent;
import org.kahina.lp.bridge.LogicProgrammingBridgeEventType;

public class LogicProgrammingTreeBehavior extends KahinaTreeBehavior
{
	private static final boolean VERBOSE = false;

	private int maxNodeLabelLength = 30;

	// call dimension is always stored in a secondary tree structure
	protected KahinaTree secondaryTree;

	// memory for construction of the primary tree
	protected int lastActiveID;

	// this stores the different breakpoint automata
	protected List<TreeAutomaton> primaryBreakpoints;
	protected List<TreeAutomaton> secondaryBreakpoints;
	protected List<TreeAutomaton> primaryWarnPoints;
	protected List<TreeAutomaton> secondaryWarnPoints;
	protected List<TreeAutomaton> skipPoints;
	protected List<TreeAutomaton> creepPoints;
	protected List<TreeAutomaton> failPoints;

	protected int stepBeingRedone = -1;
	protected Map<Integer, Integer> newStepIDByLastStepID = new HashMap<Integer, Integer>();

	public LogicProgrammingTreeBehavior(KahinaTree tree, KahinaInstance<?, ?, ?> kahina, KahinaTree secondaryTree)
	{
		super(tree, kahina);
		if (VERBOSE)
		{
			System.err.println("new LogicProgrammingTreeBehavior(" + tree + "," + kahina + "," + secondaryTree + ")");
		}
		this.secondaryTree = secondaryTree;
		this.lastActiveID = -1;
		kahina.getControl().registerListener(KahinaEventTypes.LPBRIDGE, this);
		kahina.getControl().registerListener(KahinaEventTypes.SYSTEM, this);
		kahina.getControl().registerListener(KahinaEventTypes.STEP_DESCRIPTION, this);
		kahina.getControl().registerListener(KahinaEventTypes.TREE, this);
		primaryBreakpoints = new ArrayList<TreeAutomaton>();
		initializePrimaryBreakpoints();
		compilePrimaryBreakpoints();
		secondaryBreakpoints = new ArrayList<TreeAutomaton>();
		initializeSecondaryBreakpoints();
		compileSecondaryBreakpoints();
		primaryWarnPoints = new ArrayList<TreeAutomaton>();
		initializePrimaryWarnPoints();
		compilePrimaryWarnPoints();
		secondaryWarnPoints = new ArrayList<TreeAutomaton>();
		initializeSecondaryWarnPoints();
		compileSecondaryWarnPoints();
		skipPoints = new ArrayList<TreeAutomaton>();
		initializeSkipPoints();
		compileSkipPoints();
		creepPoints = new ArrayList<TreeAutomaton>();
		initializeCreepPoints();
		compileCreepPoints();
		failPoints = new ArrayList<TreeAutomaton>();
		initializeFailPoints();
		compileFailPoints();
	}

	public int getMaxNodeLabelLength()
	{
		return maxNodeLabelLength;
	}

	/**
	 * @param maxNodeLabelLength
	 *            Length after which node labels are cut off. {@code -1} for no
	 *            cutoff.
	 */
	public void setMaxNodeLabelLength(int maxNodeLabelLength)
	{
		this.maxNodeLabelLength = maxNodeLabelLength;
	}

	/**
	 * overwrite this to fill the primaryBreakpoints list with node patterns
	 * describing at detection of which node patterns in the primary step tree
	 * the bridge is to pause leaping or skipping
	 */
	public void initializePrimaryBreakpoints()
	{

	}

	/**
	 * overwrite this to fill the secondaryBreakpoints list with node patterns
	 * describing at detection of which node patterns in the secondary step tree
	 * the bridge is to pause leaping or skipping
	 */
	public void initializeSecondaryBreakpoints()
	{

	}

	public void initializePrimaryWarnPoints()
	{

	}

	public void initializeSecondaryWarnPoints()
	{

	}

	/**
	 * overwrite this to fill the skipPoints list with node patterns describing
	 * for which nodes the bridge is to hand over a skip command to the logic
	 * programming system
	 */
	public void initializeSkipPoints()
	{

	}

	/**
	 * overwrite this to fill the creepPoints list with node patterns describing
	 * for which nodes the bridge is to automatically hand over a creep command
	 * to the logic programming system
	 */
	public void initializeCreepPoints()
	{

	}

	/**
	 * overwrite this to fill the failPoints list with node patterns describing
	 * for which nodes the bridge is to automatically hand over a fail command
	 * to the logic programming system
	 */
	public void initializeFailPoints()
	{

	}

	public void compilePrimaryBreakpoints()
	{
		this.primaryBreakpoints.clear();
		for (KahinaBreakpoint bp : ((LogicProgrammingState) kahina.getState()).getPrimaryBreakpoints())
		{
			TreeAutomaton aut = bp.compile();
			aut.setTree(object);
			aut.setController(kahina.getControl());
			aut.setConstellationMatch(false);
			this.primaryBreakpoints.add(aut);
		}
	}

	public void compileSecondaryBreakpoints()
	{
		this.secondaryBreakpoints.clear();
		for (KahinaBreakpoint bp : ((LogicProgrammingState) kahina.getState()).getSecondaryBreakpoints())
		{
			TreeAutomaton aut = bp.compile();
			aut.setTree(secondaryTree);
			aut.setController(kahina.getControl());
			aut.setConstellationMatch(false);
			this.secondaryBreakpoints.add(aut);
		}
	}

	public void compilePrimaryWarnPoints()
	{
		this.primaryWarnPoints.clear();
		for (KahinaBreakpoint bp : ((LogicProgrammingState) kahina.getState()).getPrimaryWarnPoints())
		{
			TreeAutomaton aut = bp.compile();
			aut.setTree(object);
			aut.setController(kahina.getControl());
			aut.setConstellationMatch(false);
			this.primaryWarnPoints.add(aut);
		}
	}

	public void compileSecondaryWarnPoints()
	{
		this.secondaryWarnPoints.clear();
		for (KahinaBreakpoint bp : ((LogicProgrammingState) kahina.getState()).getSecondaryWarnPoints())
		{
			TreeAutomaton aut = bp.compile();
			aut.setTree(secondaryTree);
			aut.setController(kahina.getControl());
			aut.setConstellationMatch(false);
			this.secondaryWarnPoints.add(aut);
		}
	}

	public void compileSkipPoints()
	{
		this.skipPoints.clear();
		for (KahinaBreakpoint bp : ((LogicProgrammingState) kahina.getState()).getSkipPoints())
		{
			TreeAutomaton aut = bp.compile();
			aut.setTree(secondaryTree);
			aut.setController(kahina.getControl());
			aut.setConstellationMatch(true);
			this.skipPoints.add(aut);
		}
	}

	public void compileCreepPoints()
	{
		this.creepPoints.clear();
		for (KahinaBreakpoint bp : ((LogicProgrammingState) kahina.getState()).getCreepPoints())
		{
			TreeAutomaton aut = bp.compile();
			aut.setTree(secondaryTree);
			aut.setController(kahina.getControl());
			aut.setConstellationMatch(true);
			this.creepPoints.add(aut);
		}
	}

	public void compileFailPoints()
	{
		this.failPoints.clear();
		for (KahinaBreakpoint bp : ((LogicProgrammingState) kahina.getState()).getFailPoints())
		{
			TreeAutomaton aut = bp.compile();
			aut.setTree(secondaryTree);
			aut.setController(kahina.getControl());
			aut.setConstellationMatch(true);
			this.failPoints.add(aut);
		}
	}

	/**
	 * checks for breakpoint matches caused by adding or modifying the step at
	 * stepID; causes events to be fired in the case of matches
	 */
	public void breakpointCheck(int stepID)
	{
		for (TreeAutomaton aut : primaryBreakpoints)
		{
			aut.process(stepID);
		}
		for (TreeAutomaton aut : secondaryBreakpoints)
		{
			aut.process(stepID);
		}
		for (TreeAutomaton aut : skipPoints)
		{
			aut.process(stepID);
		}
		for (TreeAutomaton aut : creepPoints)
		{
			aut.process(stepID);
		}
		for (TreeAutomaton aut : failPoints)
		{
			aut.process(stepID);
		}
		for (TreeAutomaton aut : primaryWarnPoints)
		{
			aut.process(stepID);
		}
		for (TreeAutomaton aut : secondaryWarnPoints)
		{
			aut.process(stepID);
		}
	}

	/**
	 * checks for breakpoint matches caused by failure of the step at stepID;
	 * causes events to be fired in the case of matches
	 */
	public void failureBreakpointCheck(int stepID)
	{
		for (TreeAutomaton aut : primaryBreakpoints)
		{
			aut.process(stepID);
		}
		for (TreeAutomaton aut : secondaryBreakpoints)
		{
			aut.process(stepID);
		}
		for (TreeAutomaton aut : creepPoints)
		{
			aut.process(stepID);
		}
	}

	/**
	 * checks for breakpoint matches caused by exception of the step at stepID;
	 * causes events to be fired in the case of matches
	 */
	public void exceptionBreakpointCheck(int stepID)
	{
		for (TreeAutomaton aut : primaryBreakpoints)
		{
			aut.process(stepID);
		}
		for (TreeAutomaton aut : secondaryBreakpoints)
		{
			aut.process(stepID);
		}
		for (TreeAutomaton aut : creepPoints)
		{
			aut.process(stepID);
		}
	}

	/**
	 * contains the logic by which the tree is formed out of callstacks called
	 * by the event processing routine for a KahinaTreeEvent of type "new step"
	 */
	protected void integrateIncomingNode(int stepID, int parentID)
	{
		if (VERBOSE)
			System.err.println("LogicProgrammingTreeBehavior.integratingIncomingNode(" + stepID + "," + parentID + ")");
		if (VERBOSE)
			System.err.println("\t object.addChild(" + lastActiveID + "," + stepID + ")");
		stepBeingRedone = -1;

		if (lastActiveID == -1)
		{
			object.setRootID(stepID);
		} else
		{
			object.addChild(lastActiveID, stepID);
		}

		if (parentID == -1)
		{
			secondaryTree.setRootID(stepID);
		} else
		{
			if (VERBOSE)
			{
				System.err.println("adding " + stepID + " as child of " + parentID + " in " + secondaryTree);
			}
			secondaryTree.addChild(parentID, stepID);
		}

		if (VERBOSE)
		{
			System.err.println("Tree: " + object);
			System.err.println("Secondary tree: " + secondaryTree);
		}
		lastActiveID = stepID;
		breakpointCheck(stepID);
	}

	/**
	 * integrate incoming step detail information (usually goal descriptions)
	 * into tree called by the event processing routine for a KahinaTreeEvent of
	 * type "new step"
	 * 
	 * @param stepID
	 *            - the step ID in the monitored logic programming system
	 * @param stepInfo
	 *            - the step information to be associated with the step
	 */
	public void processStepInformation(int stepID, String stepInfo)
	{
		if (VERBOSE)
			System.err.println("LogicProgrammingTreeBehavior.processStepInformation(" + stepID + ",\"" + stepInfo + "\")");
		int extID = ((LogicProgrammingState) kahina.getState()).get(stepID).getExternalID();
		String caption = makeNodeLabel(extID, stepInfo);
		object.addNode(stepID, caption, "", LogicProgrammingStepType.CALL);
		// TODO: make this unnecessary => new structure for secondary tree,
		// perhaps not a full tree model?
		secondaryTree.addNode(stepID, caption, "", LogicProgrammingStepType.CALL);
	}

	private String makeNodeLabel(int extID, String stepInfo)
	{
		if (maxNodeLabelLength > 0 && stepInfo.length() > maxNodeLabelLength)
		{
			stepInfo = stepInfo.substring(0, maxNodeLabelLength - 3) + "...";
		}
		if (extID >= 0)
		{
			return extID + " " + stepInfo;
		} else
		{
			return stepInfo;
		}
	}

	/**
	 * register and react to an incoming redo operation
	 * 
	 * @param lastStepID
	 *            - the ID of the step being redone in the monitored logic
	 *            programming system
	 */
	public void processStepRedo(int lastStepID)
	{
		if (VERBOSE)
			System.err.println("LogicProgrammingTreeBehavior.processStepRedo(" + lastStepID + ")");

		// generate a new node corresponding to the new internal step
		int newStepID = object.addNode(object.getNodeCaption(lastStepID), "", LogicProgrammingStepType.REDO);
		newStepIDByLastStepID.put(lastStepID, newStepID);
		// TODO: make this unnecessary if possible
		secondaryTree.addNode(object.getNodeCaption(lastStepID), "", LogicProgrammingStepType.REDO);
		
		// copy layer information
		int layer = secondaryTree.getLayer(lastStepID);
		if (layer != -1)
		{
			secondaryTree.setLayer(newStepID, layer);
		}

		// TODO Do we really want to do this? Pro: no bright green steps
		// flashing where there is no open choicepoint. Contra: while
		// exploring the history, no clear indication of what steps were the
		// cause for backtracking.
		if (object.getNodeStatus(lastStepID) == LogicProgrammingStepType.EXIT)
		{
			object.setNodeStatus(lastStepID, LogicProgrammingStepType.DET_EXIT);
		}

		// adapt call dimension
		int secondaryParentID = secondaryTree.getParent(lastStepID);
		if (VERBOSE)
		{
			System.err.println("Secondary parent for " + newStepID + " (copy of " + lastStepID + "): " + secondaryParentID);
		}
		while (newStepIDByLastStepID.containsKey(secondaryParentID))
		{
			secondaryParentID = newStepIDByLastStepID.get(secondaryParentID);
			if (VERBOSE)
			{
				System.err.println("Correction, secondary parent for " + newStepID + " (copy of " + lastStepID + "): " + secondaryParentID);
			}
		}
		secondaryTree.addChild(secondaryParentID, newStepID);

		// adapt control flow dimension
		int primaryParentID;
		if (stepBeingRedone == -1)
		{
			if (VERBOSE)
			{
				System.err.println("non-cascading redo");
			}
			primaryParentID = object.getParent(lastStepID);
		} else
		{
			if (VERBOSE)
			{
				System.err.println("cascading redo");
			}
			primaryParentID = stepBeingRedone;
		}
		if (VERBOSE)
		{
			System.err.println("Primary parent for " + newStepID + " (copy of " + lastStepID + "): " + primaryParentID);
		}
		object.addChild(primaryParentID, newStepID);

		lastActiveID = newStepID;
		stepBeingRedone = newStepID;
		breakpointCheck(newStepID);
	}

	/**
	 * register and react to an incoming exit operation
	 * 
	 * @param stepID
	 *            - the ID of the step that exited in the monitored logic
	 *            programming system
	 * @param deterministic
	 *            - whether the exit was deterministic
	 */
	public void processStepExit(int stepID, boolean deterministic)
	{
		stepBeingRedone = -1;
		String caption = object.getNodeCaption(stepID);
		if (caption.startsWith("block "))
		{
			object.setNodeStatus(stepID, LogicProgrammingStepType.PSEUDO_BLOCKED);
		} else if (caption.startsWith("unblock "))
		{
			object.setNodeStatus(stepID, LogicProgrammingStepType.PSEUDO_UNBLOCKED);
		} else
		{
			if (deterministic)
			{
				object.setNodeStatus(stepID, LogicProgrammingStepType.DET_EXIT);
			} else
			{
				object.setNodeStatus(stepID, LogicProgrammingStepType.EXIT);
			}
		}
		// lastActiveID = stepID;
	}

	/**
	 * registers and reacts to an incoming failed step
	 * 
	 * @param stepID
	 *            - the ID of the step that failed in the monitored logic
	 *            programming system
	 */
	public void processStepFail(int stepID)
	{
		if (VERBOSE)
		{
			System.err.println("LogicProgrammingTreeBehavior.processStepFail(" + stepID + ")");
		}
		stepBeingRedone = -1;
		object.setNodeStatus(stepID, LogicProgrammingStepType.FAIL);
		lastActiveID = object.getParent(stepID);
		failureBreakpointCheck(stepID);
	}

	public void processStepException(int stepID)
	{
		if (VERBOSE)
		{
			System.err.println("LogicProgrammingTreeBehavior.processStepException(" + stepID + ")");
		}
		stepBeingRedone = -1;
		object.setNodeStatus(stepID, LogicProgrammingStepType.EXCEPTION);
		lastActiveID = object.getParent(stepID);
		exceptionBreakpointCheck(stepID);
	}

	@Override
	public void processEvent(KahinaEvent e)
	{
		if (VERBOSE)
			System.err.println("LogicProgrammingTreeBehavior.processEvent(" + e + ")");
		if (e instanceof LogicProgrammingBridgeEvent)
		{
			processLogicProgrammingBridgeEvent((LogicProgrammingBridgeEvent) e);
		} else if (e instanceof KahinaStepDescriptionEvent)
		{
			processStepDescriptionEvent((KahinaStepDescriptionEvent) e);
		} else if (e instanceof KahinaTreeEvent)
		{
			processTreeEvent((KahinaTreeEvent) e);
		} else if (e instanceof KahinaSystemEvent)
		{
			processSystemEvent((KahinaSystemEvent) e);
		}
	}

	private void processTreeEvent(KahinaTreeEvent e)
	{
		if (e.getTreeEventType() == KahinaTreeEventType.LAYER)
		{
			if (VERBOSE)
			{
				System.err.println("Setting layer of node " + e.getFirstID() + " to " + e.getSecondID());
			}
			secondaryTree.setLayer(e.getFirstID(), e.getSecondID());
		}
	}

	public void processLogicProgrammingBridgeEvent(LogicProgrammingBridgeEvent e)
	{
		switch (e.getEventType())
		{
		case LogicProgrammingBridgeEventType.STEP_CALL:
		{
			integrateIncomingNode(e.getID(), e.getIntContent());
			break;
		}
		case LogicProgrammingBridgeEventType.SET_GOAL_DESC:
		{
			processStepInformation(e.getID(), e.getStrContent());
			break;
		}
		case LogicProgrammingBridgeEventType.STEP_REDO:
		{
			processStepRedo(e.getID());
			break;
		}
		case LogicProgrammingBridgeEventType.STEP_DET_EXIT:
		{
			processStepExit(e.getID(), true);
			break;
		}
		case LogicProgrammingBridgeEventType.STEP_NONDET_EXIT:
		{
			processStepExit(e.getID(), false);
			break;
		}
		case LogicProgrammingBridgeEventType.STEP_FAIL:
		{
			processStepFail(e.getID());
			break;
		}
		case LogicProgrammingBridgeEventType.STEP_EXCEPTION:
		{
			processStepException(e.getID());
			break;
		}
		}
	}

	public void processSystemEvent(KahinaSystemEvent e)
	{
		if (e.getSystemEventType() == KahinaSystemEvent.APPLY_BREAKPOINTS)
		{
			switch (e.getIntContent())
			{
			case KahinaBreakpointType.PRIMARY_BREAKPOINT:
			{
				compilePrimaryBreakpoints();
				break;
			}
			case KahinaBreakpointType.SECONDARY_BREAKPOINT:
			{
				compileSecondaryBreakpoints();
				break;
			}
			case KahinaBreakpointType.PRIMARY_WARN_POINT:
			{
				compilePrimaryWarnPoints();
				break;
			}
			case KahinaBreakpointType.SECONDARY_WARN_POINT:
			{
				compileSecondaryWarnPoints();
				break;
			}
			case KahinaBreakpointType.SKIP_POINT:
			{
				compileSkipPoints();
				break;
			}
			case KahinaBreakpointType.CREEP_POINT:
			{
				compileCreepPoints();
				break;
			}
			case KahinaBreakpointType.FAIL_POINT:
			{
				compileFailPoints();
				break;
			}
			}
		}
	}

	public void processStepDescriptionEvent(KahinaStepDescriptionEvent e)
	{
		int stepID = e.getStepID();
		int extID = ((LogicProgrammingState) kahina.getState()).get(stepID).getExternalID();
		object.setNodeCaption(stepID, makeNodeLabel(extID, e.getDescription()));
	}
}
