package org.kahina.tralesld;

import java.util.HashMap;

import org.kahina.core.KahinaRunner;
import org.kahina.core.data.KahinaDataHandlingMethod;
import org.kahina.core.data.chart.KahinaChart;
import org.kahina.core.data.chart.KahinaDbChart;
import org.kahina.core.data.chart.KahinaMemChart;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.gui.event.KahinaChartUpdateEvent;
import org.kahina.core.gui.event.KahinaEdgeSelectionEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.lp.LogicProgrammingState;

public class TraleSLDState extends LogicProgrammingState
{
    KahinaChart chart;
    
    //coordination between tree and chart
    HashMap<Integer, Integer> edgeToNode;
    HashMap<Integer, Integer> nodeToEdge;  
    
    public TraleSLDState(TraleSLDInstance kahina, int dataHandlingMethod)
    {
        super(kahina, dataHandlingMethod);
        if (dataHandlingMethod == KahinaDataHandlingMethod.DATABASE)
        {
            chart = new KahinaDbChart();
        }
        else
        {
            chart = new KahinaMemChart();
        }
        edgeToNode = new HashMap<Integer, Integer>();
        nodeToEdge = new HashMap<Integer, Integer>();
        KahinaRunner.getControl().registerListener("edge select", this);
        KahinaRunner.getControl().registerListener("update", this);
    }
    
    public KahinaChart getChart()
    {
        return chart;
    } 
    
    public void linkEdgeToNode(int edgeID, int nodeID)
    {
        edgeToNode.put(edgeID, nodeID);
        nodeToEdge.put(nodeID, edgeID);   
    }
    
    public void processEvent(KahinaEvent e)
    {
        if (e instanceof KahinaEdgeSelectionEvent)
        {
            processEvent((KahinaEdgeSelectionEvent) e);
        }
        if (e instanceof KahinaUpdateEvent)
        {
            processEvent((KahinaUpdateEvent) e);
        }
    }
    
    public void processEvent(KahinaEdgeSelectionEvent e)
    {
        Integer nodeID = edgeToNode.get(e.getSelectedEdge());
        if (nodeID != null) KahinaRunner.processEvent(new KahinaSelectionEvent(nodeID));
    }
    
    public void processEvent(KahinaUpdateEvent e)
    {
        super.processEvent(e);
        Integer edgeID = nodeToEdge.get(e.getSelectedStep());
        if (edgeID != null) KahinaRunner.processEvent(new KahinaChartUpdateEvent(edgeID));
    }
    
    public int getNodeForEdge(int edgeID)
    {
    	Integer result = edgeToNode.get(edgeID);
    	if (result == null)
    	{
    		return -1;
    	}
    	return result;
    }
}
