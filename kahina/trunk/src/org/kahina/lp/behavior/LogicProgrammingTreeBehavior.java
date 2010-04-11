package org.kahina.lp.behavior;

import java.util.HashSet;
import java.util.Set;

import org.kahina.core.KahinaInstance;
import org.kahina.core.behavior.KahinaTreeBehavior;
import org.kahina.core.control.KahinaController;
import org.kahina.core.control.event.KahinaEvent;
import org.kahina.core.control.event.KahinaTreeEvent;
import org.kahina.core.control.event.KahinaTreeEventType;
import org.kahina.core.control.event.LogicProgrammingBridgeEvent;
import org.kahina.core.control.event.LogicProgrammingBridgeEventType;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.lp.LogicProgrammingStepType;

public class LogicProgrammingTreeBehavior extends KahinaTreeBehavior
{
    //call dimension is always stored in a secondary tree structure
    KahinaTree secondaryTree;
    
    //memory for construction of the primary tree
    protected int lastActiveID;
    
    //store information whether steps exited or failed deterministically
    //TODO: this information must later be accessible to the drawing routine somehow
    protected Set<Integer> deterministicallyExited;
    protected Set<Integer> nonDetermBecauseOfRedo;
    
    public LogicProgrammingTreeBehavior(KahinaTree tree, KahinaController control, KahinaInstance kahina, KahinaTree secondaryTree)
    {
        super(tree, control, kahina);
        this.secondaryTree = secondaryTree;
        this.lastActiveID = -1;
        deterministicallyExited = new HashSet<Integer>();
        nonDetermBecauseOfRedo = new HashSet<Integer>();
        control.registerListener("logic programming bridge", this);
    }
    
    /**
     * contains the logic by which the tree is formed out of callstacks
     * called by the event processing routine for a KahinaTreeEvent of type "new step"
     */
    public void integrateIncomingNode(int stepID, int ancestorID)
    {    
        object.addChild(lastActiveID, stepID);
        lastActiveID = stepID;
        secondaryTree.addChild(ancestorID, stepID);
    }
    
    /**
     * integrate incoming step detail information (usually goal descriptions) into tree
     * called by the event processing routine for a KahinaTreeEvent of type "new step"
     * @param externalID - the step ID in the monitored logic programming system
     * @param stepInfo - the step information to be associated with the step
     */
    public void processStepInformation(int stepID, String stepInfo)
    {
        object.setNodeCaption(stepID, LogicProgrammingStep.get(stepID).getExternalID() + " " + stepInfo);
    }
    
    /**
     * register and react to an incoming redo operation
     * @param externalID - the ID of the step being redone in the monitored logic programming system
     */
    public void processStepRedo(int lastStepID)
    {
        nonDetermBecauseOfRedo.add(lastStepID);

        //generate a  new node corresponding to the new internal step
        int newStepID = object.addNode(LogicProgrammingStep.get(lastStepID).getGoalDesc(), "", LogicProgrammingStepType.REDO);

        //adapt call dimension
        int ancestorID = secondaryTree.getParent(lastStepID);
        secondaryTree.addChild(ancestorID, newStepID);

        //adapt control flow dimension
        int parentID = object.getParent(lastStepID);
        object.addChild(parentID, newStepID);
        
        lastActiveID = newStepID;
    }
    
    /**
     * register and react to an incoming exit operation
     * @param externalID - the ID of the step that exited in the monitored logic programming system
     * @param deterministic - whether the exit was deterministic
     */
    public void processStepExit(int stepID, boolean deterministic)
    {
        if (deterministic)
        {
            deterministicallyExited.add(stepID);
            object.setNodeStatus(stepID, LogicProgrammingStepType.DET_EXIT);
        }
        else
        {
            object.setNodeStatus(stepID, LogicProgrammingStepType.EXIT);
        }       
        lastActiveID = stepID;
    }
    
    /**
     * registers and reacts to a finished step
     * @param externalID - the ID of the step that was finished in the monitored logic programming system
     */
    public void processStepFinished(int stepID)
    {
        deterministicallyExited.add(stepID);
        lastActiveID = stepID;
    }
    
    /**
     * registers and reacts to an incoming failed step
     * @param externalID - the ID of the step that failed in the monitored logic programming system
     */
    public void processStepFail(int stepID)
    {
        deterministicallyExited.add(stepID);
        object.setNodeStatus(stepID, LogicProgrammingStepType.FAIL);   
        //TODO: determine whether behavior is correct; this operation could be risky
        lastActiveID = object.getParent(stepID);
    }
    
    public void processEvent(KahinaEvent e)
    {
        if (e instanceof KahinaTreeEvent)
        {
            processEvent((KahinaTreeEvent) e);
        }
        else if (e instanceof LogicProgrammingBridgeEvent)
        {
            processEvent((LogicProgrammingBridgeEvent) e);
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
                processStepInformation(e.getExternalID(), e.getStrContent());
                break;
            }
            case LogicProgrammingBridgeEventType.STEP_REDO:
            {
                processStepRedo(e.getExternalID());
                break;
            }
            case LogicProgrammingBridgeEventType.STEP_DET_EXIT:
            {
                processStepExit(e.getExternalID(), true);
                break;
            }
            case LogicProgrammingBridgeEventType.STEP_NONDET_EXIT:
            {
                processStepExit(e.getExternalID(), false);
                break;
            }
            case LogicProgrammingBridgeEventType.STEP_FINISHED:
            {
                processStepFinished(e.getExternalID());
                break;
            }
            case LogicProgrammingBridgeEventType.STEP_FAIL:
            {
                processStepFail(e.getExternalID());
                break;
            }
        }
    }
}
