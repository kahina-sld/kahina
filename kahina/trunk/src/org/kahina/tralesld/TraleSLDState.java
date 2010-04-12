package org.kahina.tralesld;

import java.util.HashMap;

import org.kahina.core.KahinaState;
import org.kahina.core.data.KahinaDataHandlingMethod;
import org.kahina.core.data.chart.KahinaChart;
import org.kahina.core.data.chart.KahinaDbChart;
import org.kahina.core.data.chart.KahinaMemChart;

public class TraleSLDState extends KahinaState
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
