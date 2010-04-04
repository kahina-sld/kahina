package org.tralesld.bridge;

/**
 * this class is responsible for communicating with TRALE via Jasper
 * we create an instance of this class via Jasper, and it acts as the information broker
 * the bridge can invoke an instance of Kahina via the start() method
 */

import java.util.ArrayList;
import java.util.HashSet;

import org.kahina.bridge.LogicProgrammingBridge;
import org.kahina.control.KahinaController;
import org.kahina.control.event.LogicProgrammingBridgeEvent;
import org.kahina.control.event.LogicProgrammingBridgeEventType;
import org.kahina.core.LogicProgrammingStep;
import org.kahina.core.LogicProgrammingStepType;
import org.kahina.data.DbDataManager;
import org.kahina.data.chart.KahinaChart;
import org.kahina.gui.KahinaGUI;
import org.kahina.io.database.DatabaseHandler;
import org.tralesld.control.event.TraleSLDBridgeEvent;
import org.tralesld.control.event.TraleSLDBridgeEventType;
import org.tralesld.core.TraleSLDInstance;
import org.tralesld.data.chart.TraleSLDChartEdgeStatus;

public class TraleSLDBridge extends LogicProgrammingBridge
{
    TraleSLDInstance kahina;
    ArrayList<Integer> activeEdgeStack;
    HashSet<Integer> successfulEdges;   
    
    public TraleSLDBridge(TraleSLDInstance kahina, KahinaGUI gui, KahinaController control)
    {
        super(kahina,gui,control);
        activeEdgeStack = new ArrayList<Integer>();
        successfulEdges = new HashSet<Integer>();
    }

    //TODO: processing of the sentence string should instead take place here
	public void initializeParseTrace(String parsedSentenceList)
	{

	}

	public void registerRuleApplication(int extID, int left, int right, String ruleName)
	{
        KahinaChart chart = kahina.getState().getChart();
        int newEdgeID = chart.addEdge(left, right, ruleName,TraleSLDChartEdgeStatus.ACTIVE);
        activeEdgeStack.add(0, newEdgeID);
        
        int newNodeID = kahina.getNewStepID();
        LogicProgrammingStep.setGoalDesc(newNodeID, "rule(" + ruleName + ")");
        LogicProgrammingStep.setExternalID(newNodeID, extID);
        stepIDConv.put(extID, newNodeID);
        kahina.getState().linkEdgeToNode(newEdgeID, newNodeID); 
        
        //let TraleSLDTreeBehavior do the rest
        control.processEvent(new TraleSLDBridgeEvent(TraleSLDBridgeEventType.RULE_APP, newNodeID, ruleName));
        
        //the following two actions and the structures they operate on seem to be superfluous
        //edgeRegister.put(internalStepID, currentEdge);
        //lastEdge = currentEdge;
	}

	public void registerChartEdge(int number, int left, int right, String ruleName)
	{

	}

	public void registerEdgeDependency(int motherID, int daughterID)
	{
        
	}

	public void registerMessageChunk(String chunk)
	{

	}

	public void registerMessageEnd(int externalStepID, String type)
	{

	}

	public void registerParseEnd()
	{

	}

	public void registerStepLocation(int externalStepID, int externalParentID)
	{

	}	
	
	public void registerStepFailure(int externalStepID)
	{   
        super.registerStepFailure(externalStepID);
        int stepID = convertStepID(externalStepID);
        
        String command = LogicProgrammingStep.getGoalDesc(stepID);
        // need to handle bug: step failure is called even if edge was successful
        if (command.startsWith("rule("))
        {
            int currentEdge = activeEdgeStack.remove(0);
            if (successfulEdges.contains(currentEdge))
            {
                System.err.println("Successful edge! Deleting from chart model...");
                kahina.getState().getChart().removeEdge(currentEdge);
                //TODO: was SUCCESS in the original; what exactly is the difference?
                LogicProgrammingStep.setType(stepID, LogicProgrammingStepType.EXIT);
            }
            // current rule application failed; adapt chart accordingly
            else
            {
                System.err.println("Failed edge! Leaving it on the chart as junk...");
                kahina.getState().getChart().setEdgeStatus(currentEdge, TraleSLDChartEdgeStatus.FAILED);
                //TODO: devise a way of separating edge activation from status
                //currentEdge.active = false;
                LogicProgrammingStep.setType(stepID, LogicProgrammingStepType.FAIL);
            }
            // move up one level in overview tree (really necessary?)
            //currentOverviewTreeNode = tracer.overviewTraceView.treeNodes.get(currentOverviewTreeNode).getParent();
            //lastEdge = edgeRegister.getData(currentOverviewTreeNode);
        }
	}
	
	public void registerStepFinished(int extID)
	{
        int stepID = convertStepID(extID);
        control.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_FINISHED, stepID));
        //TODO: this has to be covered by the TraleSLDTreeBehavior
        if (LogicProgrammingStep.getGoalDesc(stepID).startsWith("rule_close"))
        {
            //TODO: in the original, this was SUCCESS - find out why
            LogicProgrammingStep.setType(stepID, LogicProgrammingStepType.DET_EXIT);
            // move up one level in overview tree (really necessary?)
            //currentOverviewTreeNode = tracer.overviewTraceView.treeNodes.get(currentOverviewTreeNode).getParent();
            //lastEdge = edgeRegister.getData(currentOverviewTreeNode);
        }
	}

	public void registerBlockedPseudoStepInformation(int externalStepID, String goal)
	{

	}

	public void registerUnblockedPseudoStepInformation(int externalStepID, int externalBlockedPseudoStepID, String goal)
	{

	}

}
