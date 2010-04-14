package org.kahina.tralesld.bridge;

/**
 * this class is responsible for communicating with TRALE via Jasper
 * we create an instance of this class via Jasper, and it acts as the information broker
 * the bridge can invoke an instance of Kahina via the start() method
 */

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.chart.KahinaChart;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.util.PrologUtilities;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.lp.LogicProgrammingStepType;
import org.kahina.lp.bridge.LogicProgrammingBridge;
import org.kahina.lp.event.LogicProgrammingBridgeEvent;
import org.kahina.lp.event.LogicProgrammingBridgeEventType;
import org.kahina.tralesld.TraleSLDInstance;
import org.kahina.tralesld.control.event.TraleSLDBridgeEvent;
import org.kahina.tralesld.control.event.TraleSLDBridgeEventType;
import org.kahina.tralesld.data.chart.TraleSLDChartEdgeStatus;

public class TraleSLDBridge extends LogicProgrammingBridge
{
    TraleSLDInstance kahina;
    ArrayList<Integer> activeEdgeStack;
    HashSet<Integer> successfulEdges;   
    
    public TraleSLDBridge(TraleSLDInstance kahina, KahinaGUI gui, KahinaController control)
    {
        super(kahina,gui,control);
        this.kahina = kahina;
        activeEdgeStack = new ArrayList<Integer>();
        successfulEdges = new HashSet<Integer>();
    }

    //TODO: processing of the sentence string should instead take place here
	public void initializeParseTrace(String parsedSentenceList)
	{
        System.err.println("initializeParseTrace(\"" + parsedSentenceList + "\")");
        List<String> wordList = PrologUtilities.parsePrologStringList(parsedSentenceList);
        LogicProgrammingStep newStep = new LogicProgrammingStep();
        newStep.setGoalDesc("init");
        newStep.setExternalID(0);
        stepIDConv.put(0, newStep.getID());
        newStep.store();
        control.processEvent(new TraleSLDBridgeEvent(TraleSLDBridgeEventType.INIT, newStep.getID(), wordList.toString()));
        currentID = newStep.getID();
        control.processEvent(new KahinaSelectionEvent(newStep.getID()));
	}

	public void registerRuleApplication(int extID, int left, int right, String ruleName)
	{
        System.err.println("registerRuleApplication(" + extID + "," + left + "," + right + ",\"" + ruleName + "\")");
        KahinaChart chart = kahina.getState().getChart();
        int newEdgeID = chart.addEdge(left, right, ruleName,TraleSLDChartEdgeStatus.ACTIVE);
        activeEdgeStack.add(0, newEdgeID);
        
        LogicProgrammingStep newStep = new LogicProgrammingStep();
        newStep.setGoalDesc("rule(" + ruleName + ")");
        newStep.setExternalID(extID);
        stepIDConv.put(extID, newStep.getID());
        kahina.getState().linkEdgeToNode(newEdgeID, newStep.getID()); 
        newStep.store();
        
        //let TraleSLDTreeBehavior do the rest
        control.processEvent(new TraleSLDBridgeEvent(TraleSLDBridgeEventType.RULE_APP, newStep.getID(), ruleName));
        
        //the following two actions and the structures they operate on seem to be superfluous
        //edgeRegister.put(internalStepID, currentEdge);
        //lastEdge = currentEdge;
	}

	public void registerChartEdge(int number, int left, int right, String ruleName)
	{
        System.err.println("registerChartEdge(" + number + "," + left + "," + right + ",\"" + ruleName + "\")");
	}

	public void registerEdgeDependency(int motherID, int daughterID)
	{
        System.err.println("registerEdgeDependency(" + motherID + "," + daughterID + ")");
	}

	public void registerMessageChunk(String chunk)
	{
        System.err.println("registerMessageChunk(\"" + chunk + "\")");
	}

	public void registerMessageEnd(int extID, String type)
	{
        System.err.println("registerMessageChunk(" + extID + ",\"" + type + "\")");
	}

	public void registerParseEnd()
	{
        System.err.println("registerParseEnd()");
	}
	
	public void registerStepFailure(int externalStepID)
	{   
        System.err.println("registerStepFailure(" + externalStepID + ")");
        super.registerStepFailure(externalStepID);
        int stepID = convertStepID(externalStepID);
        
        String command = LogicProgrammingStep.get(stepID).getGoalDesc();
        // need to handle bug: step failure is called even if edge was successful
        if (command.startsWith("rule("))
        {
            int currentEdge = activeEdgeStack.remove(0);
            if (successfulEdges.contains(currentEdge))
            {
                System.err.println("Successful edge! Deleting from chart model...");
                kahina.getState().getChart().removeEdge(currentEdge);
                //TODO: was SUCCESS in the original; what exactly is the difference?
                LogicProgrammingStep.get(stepID).setType(LogicProgrammingStepType.EXIT);
            }
            // current rule application failed; adapt chart accordingly
            else
            {
                System.err.println("Failed edge! Leaving it on the chart as junk...");
                kahina.getState().getChart().setEdgeStatus(currentEdge, TraleSLDChartEdgeStatus.FAILED);
                //TODO: devise a way of separating edge activation from status
                //currentEdge.active = false;
                LogicProgrammingStep.get(stepID).setType(LogicProgrammingStepType.FAIL);
            }
            // move up one level in overview tree (really necessary?)
            //currentOverviewTreeNode = tracer.overviewTraceView.treeNodes.get(currentOverviewTreeNode).getParent();
            //lastEdge = edgeRegister.getData(currentOverviewTreeNode);
        }
        currentID = stepID;
        control.processEvent(new KahinaSelectionEvent(stepID));
	}
	
	public void registerStepFinished(int extID)
	{
        System.err.println("registerStepFinished(" + extID + ")");
        int stepID = convertStepID(extID);
        control.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_FINISHED, stepID));
        //TODO: this has to be covered by the TraleSLDTreeBehavior
        if (LogicProgrammingStep.get(stepID).getGoalDesc().startsWith("rule_close"))
        {
            //TODO: in the original, this was SUCCESS - find out why
            LogicProgrammingStep.get(stepID).setType(LogicProgrammingStepType.DET_EXIT);
            // move up one level in overview tree (really necessary?)
            //currentOverviewTreeNode = tracer.overviewTraceView.treeNodes.get(currentOverviewTreeNode).getParent();
            //lastEdge = edgeRegister.getData(currentOverviewTreeNode);
        }
	}

	public void registerBlockedPseudoStepInformation(int extID, String goal)
	{
        System.err.println("registerBlockedPseudoStepInformation(" + extID + ",\"" + goal + "\")");
	}

	public void registerUnblockedPseudoStepInformation(int extID, int extBlockedPseudoStepID, String goal)
	{
        System.err.println("registerUnblockedPseudoStepInformation(" + extID + "," + extBlockedPseudoStepID + ",\"" + goal + "\")");
	}

}
