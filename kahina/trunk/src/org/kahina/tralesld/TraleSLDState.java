package org.kahina.tralesld;

import java.util.HashMap;

import org.kahina.core.KahinaState;
import org.kahina.core.data.chart.KahinaChart;

public class TraleSLDState extends KahinaState
{
    KahinaChart chart;
    
    //coordination between tree and chart
    HashMap<Integer, Integer> edgeToNode;
    HashMap<Integer, Integer> nodeToEdge;  
    
    public TraleSLDState(TraleSLDInstance kahina, int dataHandlingMethod)
    {
        super(kahina, dataHandlingMethod);
        edgeToNode = new HashMap<Integer, Integer>();
        nodeToEdge = new HashMap<Integer, Integer>();
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
}
