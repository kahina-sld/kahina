package org.kahina.lp.bridge;

import java.util.HashMap;

import org.kahina.core.KahinaRunner;
import org.kahina.core.bridge.KahinaBridge;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.event.KahinaAbortEvent;
import org.kahina.core.event.KahinaControlEvent;
import org.kahina.core.event.KahinaEventTypes;
import org.kahina.core.event.KahinaTreeEvent;
import org.kahina.core.event.KahinaTreeEventType;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.lp.LogicProgrammingStepType;
import org.kahina.lp.data.text.LogicProgrammingLineReference;
import org.kahina.lp.event.LogicProgrammingBridgeEvent;
import org.kahina.lp.event.LogicProgrammingBridgeEventType;

public class LogicProgrammingBridge extends KahinaBridge
{  
    private static final boolean verbose = false;
    
    //a dynamic map from external step IDs to most recent corresponding tree nodes
    protected HashMap<Integer,Integer> stepIDConv;
    
    //always contains the internal ID of the most recent step
    protected int currentID = -1;
    
    //always contains the interal ID of the selected step
    protected int selectedID = -1;
    
    //store the state of the bridge, determining the next result of getPressedButton()
    protected char bridgeState;
    
    //in skip mode, this is the internal step ID of the step we are skipping
    int skipID = -1;
    
    LogicProgrammingState state; 
    
    public LogicProgrammingBridge(LogicProgrammingState state)
    {
    	super();
    	this.state = state;
        stepIDConv = new HashMap<Integer,Integer>();
        KahinaRunner.getControl().registerListener(KahinaEventTypes.ABORT, this);
        KahinaRunner.getControl().registerListener(KahinaEventTypes.SELECTION, this);
        if (verbose) System.err.println("new LogicProgrammingBridge()");
    }
    
    /**
     * convert external step IDs to internal IDs corresponding to tree nodes
     * uses entries in stepIDConv table, extending it together with the tree if no entry was found
     * @return an internal step ID corresponding to the external ID
     */
    public int convertStepID(int extID)
    {
    	if (verbose) System.err.println("LogicProgrammingBridge.convertStepID(" + extID + ")");
        Integer intID = stepIDConv.get(extID);
        if (intID == null)
        {
            LogicProgrammingStep newStep = generateStep();
            intID = newStep.getID();
            newStep.setExternalID(extID);
            newStep.storeCaching();
            stepIDConv.put(extID, intID);
        }
        if (verbose) System.err.println("LogicProgrammingBridge.convertStepID(" + extID + ") = " + intID);
        return intID;
    }
    
    public void registerStepInformation(int extID, String nodeLabel, String consoleMessage)
    {
        try
        {
            if (verbose) System.err.println("LogicProgrammingBridge.registerStepInformation(" + extID + ",\"" + nodeLabel + "\")");
            int stepID = convertStepID(extID);
            LogicProgrammingStep step = LogicProgrammingStep.get(stepID);
            step.setGoalDesc(nodeLabel);
            step.setSourceCodeLocation(LogicProgrammingStep.get(currentID).getSourceCodeLocation());
            step.storeCaching();
            KahinaRunner.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.SET_GOAL_DESC, stepID, nodeLabel));
            currentID = stepID;
            
            state.consoleMessage(stepID, extID, LogicProgrammingStepType.CALL ,consoleMessage);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            System.exit(1);
        }
    }
    
    public void registerStepSourceCodeLocation(int extID, String absolutePath, int lineNumber)
    {
        try
        {
            if (verbose) System.err.println("LogicProgrammingBridge.registerStepSourceCodeLocation(" + extID + ",\"" + absolutePath + "\"," + lineNumber + ")");
            int stepID = convertStepID(extID);
            LogicProgrammingStep step = LogicProgrammingStep.get(stepID);
            step.setSourceCodeLocation(new KahinaSourceCodeLocation(absolutePath, lineNumber - 1, stepID));
            currentID = stepID;
            step.storeCaching();
        }
        catch (Exception e)
        {
            e.printStackTrace();
            System.exit(1);
        }
    }
    
    public void registerStepLocation(int extID, int parentID)
    {
        try
        {
            if (verbose) System.err.println("LogicProgrammingBridge.registerStepLocation(" + extID + "," + parentID + ")");
            int stepID = convertStepID(extID);
            KahinaRunner.processEvent(new KahinaTreeEvent(KahinaTreeEventType.NEW_NODE, stepID, convertStepID(parentID)));
            currentID = stepID;
            if (bridgeState == 'n') KahinaRunner.processEvent(new KahinaSelectionEvent(stepID));
        }
        catch (Exception e)
        {
            e.printStackTrace();
            System.exit(1);
        }
    }
    
    public void registerStepRedo(int extID)
    {
        try
        {
            if (verbose) System.err.println("LogicProgrammingBridge.registerStepRedo(" + extID + ")");
            int lastStepID = convertStepID(extID);
            LogicProgrammingStep lastStep = LogicProgrammingStep.get(lastStepID);
            LogicProgrammingStep newStep = lastStep.copy();
            newStep.storeCaching();
            int newStepID = newStep.getID();
            stepIDConv.put(extID, newStepID);
            KahinaRunner.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_REDO, lastStepID));
            currentID = newStepID;
            if (bridgeState == 'n') KahinaRunner.processEvent(new KahinaSelectionEvent(newStepID));
            
            LogicProgrammingLineReference ref = state
                                                .getConsoleLineRefForStep(lastStepID)
                                                .generatePortVariant(LogicProgrammingStepType.REDO);
            ref.step = newStepID;
            ref.store();
            state.consoleMessage(ref);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            System.exit(1);
        }
    }
    
    public void registerStepExit(int extID, boolean deterministic)
    {
        try
        {
            if (verbose) System.err.println("LogicProgrammingBridge.registerStepExit(" + extID + "," + deterministic + ")");
            int stepID = convertStepID(extID);
            if (deterministic)
            {
                KahinaRunner.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_DET_EXIT, stepID));
            }
            else
            {
                KahinaRunner.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_NONDET_EXIT, stepID));
            }
            currentID = stepID;
            if (bridgeState == 'n') KahinaRunner.processEvent(new KahinaSelectionEvent(stepID));
                     
            LogicProgrammingLineReference ref = null;
            if (deterministic) ref = state.getConsoleLineRefForStep(stepID)
                                     .generatePortVariant(LogicProgrammingStepType.DET_EXIT);
            else
            {
                ref = state.getConsoleLineRefForStep(stepID)
                      .generatePortVariant(LogicProgrammingStepType.EXIT);
            }
            ref.store();
            state.consoleMessage(ref);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            System.exit(1);
        }
    }
    
    public void registerStepFailure(int extID)
    {
        try
        {
            if (verbose) System.err.println("LogicProgrammingBridge.registerStepFailure(" + extID + ")");
            int stepID = convertStepID(extID);
            KahinaRunner.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_FAIL, stepID));
            currentID = stepID;
            if (bridgeState == 'n') KahinaRunner.processEvent(new KahinaSelectionEvent(stepID));
            LogicProgrammingLineReference ref = state.getConsoleLineRefForStep(stepID)
                                                .generatePortVariant(LogicProgrammingStepType.FAIL);
            ref.store();
            state.consoleMessage(ref);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            System.exit(1);
        }
    }
    
    public LogicProgrammingStep generateStep()
    {
    	if (verbose) System.err.println("LogicProgrammingBridge.generateStep()");
        return new LogicProgrammingStep();
    }
       
    public char getPressedButton()
    {
        switch (bridgeState)
        {
            case 'n':
            {
                return 'n';
            }
            case 'p':
            {
                return 'n';
            }
            case 'q':
            {
                return 'n';
            }
            case 'c':
            {
                bridgeState = 'n';
                return 'c';
            }
            case 'f':
            {
                bridgeState = 'n';
                return 'f';
            }
            case 'l':
            {
                bridgeState = 'l';
                return 'c';
            }
            case 't':
            {
                bridgeState = 's';
                return 'c';
            }
            case 's':
            {
                if (skipID == currentID)
                {
                    skipID = -1;
                    bridgeState = 'n';
                    KahinaRunner.processEvent(new KahinaSelectionEvent(currentID));
                    return 'n';
                }
                else
                {
                    return 'c';
                }
            }
            case 'a':
            {
            	return 'a';
            }
            default:
            {
                bridgeState = 'n';
                return 'n';
            }
        }
    }
    
    @Override
    protected void processEvent(KahinaAbortEvent e)
    {
    	bridgeState = 'a';
    }
    
    @Override
    protected void processEvent(KahinaControlEvent e)
    {
        String command = e.getCommand();
        if (command.equals("creep"))
        {
            if (bridgeState == 'n')
            {
                bridgeState = 'c';
            }
            else if (bridgeState == 'p')
            {
                skipID = -1;
                bridgeState = 'c';
            }
            else if (bridgeState == 'q')
            {
                skipID = -1;
                bridgeState = 'c';
            }
            else if (bridgeState == 'l')
            {
                skipID = -1;
                bridgeState = 'n';
            }
        }
        else if (command.equals("fail"))
        {
            if (bridgeState == 'n')
            {
                bridgeState = 'f';
            }
            else if (bridgeState == 'p')
            {
                skipID = -1;
                bridgeState = 'f';
            }
            else if (bridgeState == 'q')
            {
                skipID = -1;
                bridgeState = 'f';
            }
        }
        else if (command.equals("skip"))
        {
            if (bridgeState == 'n')
            {
                bridgeState = 't';
                if (selectedID == -1)
                {
                	skipID = currentID;
                } else
                {
                	skipID = selectedID;
                }
            }
            else if (bridgeState == 'p')
            {
                bridgeState = 't';
            }
            else if (bridgeState == 'q')
            {
                bridgeState = 't';
                skipID = currentID;
            }
        }
        else if (command.equals("leap"))
        {
            if (bridgeState == 'n')
            {
                bridgeState = 'l';
            }
            else if (bridgeState == 'p')
            {
                bridgeState = 'l';
                skipID = -1;
            }
            else if (bridgeState == 'q')
            {
                bridgeState = 'l';
                skipID = -1;
            }
        }
        else if (command.equals("(un)pause"))
        {
            if (bridgeState == 't')
            {
                bridgeState = 'p';
            }
            else if (bridgeState == 's')
            {
                bridgeState = 'q';
            }
            else if (bridgeState == 'p')
            {
                bridgeState = 't';
            }
            else if (bridgeState == 'q')
            {
                bridgeState = 's';
            }
        }
    }
    
    @Override
    protected void processEvent(KahinaSelectionEvent e)
    {
    	selectedID = e.getSelectedStep();
    }
}
