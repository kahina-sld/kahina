package org.kahina.lp.behavior;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaRunner;
import org.kahina.core.behavior.KahinaTreeBehavior;
import org.kahina.core.breakpoint.KahinaBreakpoint;
import org.kahina.core.breakpoint.KahinaBreakpointType;
import org.kahina.core.breakpoint.TreeAutomaton;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.event.KahinaSystemEvent;
import org.kahina.core.event.KahinaTreeEvent;
import org.kahina.core.event.KahinaTreeEventType;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.lp.LogicProgrammingStepType;
import org.kahina.lp.event.LogicProgrammingBridgeEvent;
import org.kahina.lp.event.LogicProgrammingBridgeEventType;

public class LogicProgrammingTreeBehavior extends KahinaTreeBehavior
{
	private static final boolean verbose = false;

	// call dimension is always stored in a secondary tree structure
	protected KahinaTree secondaryTree;

	// memory for construction of the primary tree
	protected int lastActiveID;

	// store information whether steps exited or failed deterministically
	// TODO: this information must later be accessible to the drawing routine
	// somehow
	protected Set<Integer> deterministicallyExited;
	protected Set<Integer> nonDetermBecauseOfRedo;

	// this stores the different breakpoint automata
	protected List<TreeAutomaton> primaryBreakpoints;
	protected List<TreeAutomaton> secondaryBreakpoints;
	protected List<TreeAutomaton> skipPoints;
    protected List<TreeAutomaton> creepPoints;
    protected List<TreeAutomaton> failPoints;

	public LogicProgrammingTreeBehavior(KahinaTree tree, KahinaInstance<?, ?, ?> kahina, KahinaTree secondaryTree)
	{
		super(tree, kahina);
		this.secondaryTree = secondaryTree;
		this.lastActiveID = -1;
		deterministicallyExited = new HashSet<Integer>();
		nonDetermBecauseOfRedo = new HashSet<Integer>();
		KahinaRunner.getControl().registerListener("logic programming bridge", this);
		KahinaRunner.getControl().registerListener("system", this);
		if (verbose)
			System.err.println("new LogicProgrammingTreeBehavior(" + tree + "," + "," + kahina + "," + secondaryTree + ")");
		primaryBreakpoints = new ArrayList<TreeAutomaton>();
		initializePrimaryBreakpoints();
		compilePrimaryBreakpoints();
		secondaryBreakpoints = new ArrayList<TreeAutomaton>();
		initializeSecondaryBreakpoints();
		compileSecondaryBreakpoints();
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
     * for which nodes the bridge is to automatically hand over a creep command to the logic
     * programming system
     */
    public void initializeCreepPoints()
    {

    }
    
    /**
     * overwrite this to fill the failPoints list with node patterns describing
     * for which nodes the bridge is to automatically hand over a fail command to the logic
     * programming system
     */
    public void initializeFailPoints()
    {

    }

	public void compilePrimaryBreakpoints()
	{
		this.primaryBreakpoints.clear();
		for (KahinaBreakpoint bp : kahina.getState().getPrimaryBreakpoints())
		{
			TreeAutomaton aut = bp.compile();
			aut.setTree(object);
			aut.setController(KahinaRunner.getControl());
			aut.setConstellationMatch(false);
			this.primaryBreakpoints.add(aut);
		}
	}

	public void compileSecondaryBreakpoints()
	{
		this.secondaryBreakpoints.clear();
		for (KahinaBreakpoint bp : kahina.getState().getSecondaryBreakpoints())
		{
			TreeAutomaton aut = bp.compile();
			aut.setTree(secondaryTree);
			aut.setController(KahinaRunner.getControl());
			aut.setConstellationMatch(false);
			this.secondaryBreakpoints.add(aut);
		}
	}

	public void compileSkipPoints()
	{
		this.skipPoints.clear();
		for (KahinaBreakpoint bp : kahina.getState().getSkipPoints())
		{
			TreeAutomaton aut = bp.compile();
			aut.setTree(secondaryTree);
			aut.setController(KahinaRunner.getControl());
			aut.setConstellationMatch(true);
			this.skipPoints.add(aut);
		}
	}
    
    public void compileCreepPoints()
    {
        this.creepPoints.clear();
        for (KahinaBreakpoint bp : kahina.getState().getCreepPoints())
        {
            TreeAutomaton aut = bp.compile();
            aut.setTree(secondaryTree);
            aut.setController(KahinaRunner.getControl());
            aut.setConstellationMatch(true);
            this.creepPoints.add(aut);
        }
    }
    
    public void compileFailPoints()
    {
        this.failPoints.clear();
        for (KahinaBreakpoint bp : kahina.getState().getFailPoints())
        {
            TreeAutomaton aut = bp.compile();
            aut.setTree(secondaryTree);
            aut.setController(KahinaRunner.getControl());
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
	}
    
    /**
     * checks for breakpoint matches caused by failure of the step at
     * stepID; causes events to be fired in the case of matches
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
	 * contains the logic by which the tree is formed out of callstacks called
	 * by the event processing routine for a KahinaTreeEvent of type "new step"
	 */
	public void integrateIncomingNode(int stepID, int ancestorID)
	{
		if (verbose)
			System.err.println("LogicProgrammingTreeBehavior.integratingIncomingNode(" + stepID + "," + ancestorID + ")");
		if (verbose)
			System.err.println("\t object.addChild(" + lastActiveID + "," + stepID + ")");
		object.addChild(lastActiveID, stepID);
		// if (verbose) System.err.println(object.exportXML());
		lastActiveID = stepID;
		secondaryTree.addChild(ancestorID, stepID);
		// if (verbose) System.err.println(secondaryTree.exportXML());
		breakpointCheck(stepID);
	}

	private void processUnblocked(int pseudoStepInternalID, String description)
    {
		// 1. process information
    	object.addNode(pseudoStepInternalID, description, "", LogicProgrammingStepType.PSEUDO_UNBLOCKED);
    	// TODO make this unnecessary, see processStepInformation
    	secondaryTree.addNode(pseudoStepInternalID, description, "", LogicProgrammingStepType.PSEUDO_UNBLOCKED);
    	// 2. integrate node
    	if (verbose)
    	{
    		System.err.println("Adding pseudostep " + pseudoStepInternalID + " as child of " + lastActiveID);
    	}
    	object.addChild(lastActiveID, pseudoStepInternalID);
    	secondaryTree.addChild(lastActiveID, pseudoStepInternalID);
    	lastActiveID = pseudoStepInternalID;
    	breakpointCheck(pseudoStepInternalID);
    }

	/**
	 * integrate incoming step detail information (usually goal descriptions)
	 * into tree called by the event processing routine for a KahinaTreeEvent of
	 * type "new step"
	 * 
	 * @param externalID
	 *            - the step ID in the monitored logic programming system
	 * @param stepInfo
	 *            - the step information to be associated with the step
	 */
	public void processStepInformation(int stepID, String stepInfo)
	{
		if (verbose)
			System.err.println("LogicProgrammingTreeBehavior.processStepInformation(" + stepID + ",\"" + stepInfo + "\")");
		String caption = LogicProgrammingStep.get(stepID).getExternalID() + " " + stepInfo;
		object.addNode(stepID, caption, "", LogicProgrammingStepType.CALL);
		// TODO: make this unnecessary => new structure for secondary tree,
		// perhaps not a full tree model?
		secondaryTree.addNode(stepID, caption, "", LogicProgrammingStepType.CALL);
	}

	/**
	 * register and react to an incoming redo operation
	 * 
	 * @param externalID
	 *            - the ID of the step being redone in the monitored logic
	 *            programming system
	 */
	public void processStepRedo(int lastStepID)
	{
		if (verbose)
			System.err.println("LogicProgrammingTreeBehavior.processStepRedo(" + lastStepID + ")");

		nonDetermBecauseOfRedo.add(lastStepID);

		// generate a new node corresponding to the new internal step
		int newStepID = object.addNode(object.getNodeCaption(lastStepID), "", LogicProgrammingStepType.REDO);
		// TODO: make this unnecessary if possible
		secondaryTree.addNode(object.getNodeCaption(lastStepID), "", LogicProgrammingStepType.REDO);

		object.setNodeStatus(lastStepID, LogicProgrammingStepType.DET_EXIT);

		// adapt call dimension
		int ancestorID = secondaryTree.getParent(lastStepID);
		secondaryTree.addChild(ancestorID, newStepID);

		// adapt control flow dimension
		int parentID = object.getParent(lastStepID);
		object.addChild(parentID, newStepID);

		lastActiveID = newStepID;
		breakpointCheck(newStepID);
	}

	/**
	 * register and react to an incoming exit operation
	 * 
	 * @param externalID
	 *            - the ID of the step that exited in the monitored logic
	 *            programming system
	 * @param deterministic
	 *            - whether the exit was deterministic
	 */
	public void processStepExit(int stepID, boolean deterministic)
	{
		if (deterministic)
		{
			deterministicallyExited.add(stepID);
			object.setNodeStatus(stepID, LogicProgrammingStepType.DET_EXIT);
		} else
		{
			object.setNodeStatus(stepID, LogicProgrammingStepType.EXIT);
		}
		// lastActiveID = stepID;
	}

	/**
	 * registers and reacts to an incoming failed step
	 * 
	 * @param externalID
	 *            - the ID of the step that failed in the monitored logic
	 *            programming system
	 */
	public void processStepFail(int stepID)
	{
		if (verbose)
			System.err.println("LogicProgrammingTreeBehavior.processStepFail(" + stepID + ")");
		deterministicallyExited.add(stepID);
		object.setNodeStatus(stepID, LogicProgrammingStepType.FAIL);
		lastActiveID = object.getParent(stepID);
        failureBreakpointCheck(stepID);
	}

	public void processEvent(KahinaEvent e)
	{
		if (verbose)
			System.err.println("LogicProgrammingTreeBehavior.processEvent(" + e + ")");
		if (e instanceof KahinaTreeEvent)
		{
			processEvent((KahinaTreeEvent) e);
		} else if (e instanceof LogicProgrammingBridgeEvent)
		{
			processEvent((LogicProgrammingBridgeEvent) e);
		} else if (e instanceof KahinaSystemEvent)
		{
			processEvent((KahinaSystemEvent) e);
		}
	}

	public void processEvent(KahinaTreeEvent e)
	{
		switch (e.getTreeEventType())
		{
			case KahinaTreeEventType.NEW_NODE:
			{
				integrateIncomingNode(e.getFirstID(), e.getSecondID());
				break;
			}
		}
	}

	public void processEvent(LogicProgrammingBridgeEvent e)
	{
		switch (e.getEventType())
		{
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
			case LogicProgrammingBridgeEventType.UNBLOCKED:
			{
				processUnblocked(e.getID(), e.getStrContent());
			}
		}
	}

	public void processEvent(KahinaSystemEvent e)
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
}
