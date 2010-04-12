package org.kahina.lp.bridge;

import java.util.HashMap;

import org.kahina.core.KahinaInstance;
import org.kahina.core.bridge.KahinaBridge;
import org.kahina.core.control.KahinaController;
import org.kahina.core.event.KahinaControlEvent;
import org.kahina.core.event.KahinaTreeEvent;
import org.kahina.core.event.KahinaTreeEventType;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.lp.LogicProgrammingStepType;
import org.kahina.lp.event.LogicProgrammingBridgeEvent;
import org.kahina.lp.event.LogicProgrammingBridgeEventType;

public class LogicProgrammingBridge extends KahinaBridge
{   
    //a dynamic map from external step IDs to most recent corresponding tree nodes
    protected HashMap<Integer,Integer> stepIDConv;
    
    //store the next result of getPressedButton()
    char pressedButton;
    
    public LogicProgrammingBridge(KahinaInstance kahina, KahinaGUI gui, KahinaController control)
    {
        super(kahina, gui, control);
        stepIDConv = new HashMap<Integer,Integer>();
    }
    
    /**
     * convert external step IDs to internal IDs corresponding to tree nodes
     * uses entries in stepIDConv table, extending it together with the tree if no entry was found
     * @return an internal step ID corresponding to the external ID
     */
    public int convertStepID(int extID)
    {
        Integer intID = stepIDConv.get(extID);
        if (intID == null)
        {
            intID = kahina.getNewStepID();
            LogicProgrammingStep.get(intID).setExternalID(extID);
            stepIDConv.put(extID, intID);
        }
        return intID;
    }
    
    public void registerStepInformation(int extID, String stepInfo)
    {
        int stepID = convertStepID(extID);
        LogicProgrammingStep.get(stepID).setGoalDesc(stepInfo);
        control.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.SET_GOAL_DESC, stepID, stepInfo));
    }
    
    public void registerStepSourceCodeLocation(int extID, String absolutePath, int lineNumber)
    {
        int stepID = convertStepID(extID);
        //LogicProgrammingStep.setSourceCodeLocation(stepID, absolutePath, lineNumber - 1);
    }
    
    public void registerStepLocation(int stepID, int parentID)
    {
        control.processEvent(new KahinaTreeEvent(KahinaTreeEventType.NEW_NODE, convertStepID(stepID), convertStepID(parentID)));
    }
    
    public void registerStepRedo(int extID)
    {
        int lastStepID = convertStepID(extID);
        int newStepID = kahina.getNewStepID();
        LogicProgrammingStep newStep = LogicProgrammingStep.get(lastStepID).copy();
        newStep.setType(LogicProgrammingStepType.REDO);
        stepIDConv.put(extID, newStepID);
        control.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_REDO, lastStepID));
    }
    
    public void registerStepExit(int extID, boolean deterministic)
    {
        LogicProgrammingStep step = LogicProgrammingStep.get(convertStepID(extID));
        if (deterministic)
        {
            step.setType(LogicProgrammingStepType.DET_EXIT);
            control.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_DET_EXIT, step.getID()));
        }
        else
        {
            control.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_NONDET_EXIT, step.getID()));
            step.setType(LogicProgrammingStepType.EXIT);
        }
        step.store();
    }
    
    public void registerStepFinished(int extID)
    {
        int stepID = convertStepID(extID);
        control.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_FINISHED, stepID));
    }
    
    public void registerStepFailure(int extID)
    {
        int stepID = convertStepID(extID);
        LogicProgrammingStep.get(stepID).setType(LogicProgrammingStepType.FAIL);   
        LogicProgrammingStep.get(stepID).store();  
        control.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_FAIL, stepID));
    }
       
    public char getPressedButton()
    {
        switch (pressedButton)
        {
            case 'n':
            {
                return 'n';
            }
            case 'c':
            {
                pressedButton = 'n';
                return 'c';
            }
            case 'f':
            {
                pressedButton = 'f';
                return 'f';
            }
            default:
            {
                pressedButton = 'n';
                return 'n';
            }
        }
    }
    
    public void processEvent(KahinaControlEvent e)
    {
        String command = e.getCommand();
        if (command.equals("creep"))
        {
            pressedButton = 'c';
        }
        else if (command.equals("fail"))
        {
            pressedButton = 'f';
        }
        else if (command.equals("skip"))
        {
            
        }
        else if (command.equals("leap"))
        {
            
        }
    }
}
