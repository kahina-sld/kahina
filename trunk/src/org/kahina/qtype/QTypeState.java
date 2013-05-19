package org.kahina.qtype;

import java.util.HashMap;

import org.kahina.core.data.chart.KahinaChart;
import org.kahina.core.data.chart.KahinaMemChart;
import org.kahina.lp.LogicProgrammingState;

public class QTypeState extends LogicProgrammingState
{
    KahinaChart chart;
    
    //coordination between step tree and chart
    HashMap<Integer, Integer> edgeToNode;
    HashMap<Integer, Integer> nodeToEdge;
    
    public QTypeState(QTypeDebuggerInstance kahina)
    {
        super(kahina);
        System.err.println("New QTypeState()");
        System.err.println("Current heap size: " + Runtime.getRuntime().totalMemory());
        
        chart = new KahinaMemChart();
            
        edgeToNode = new HashMap<Integer, Integer>();
        nodeToEdge = new HashMap<Integer, Integer>();
    }
    
    public void initialize()
    {
        super.initialize();
        chart = new KahinaMemChart();
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

    public int getNodeForEdge(int edgeID)
    {
        Integer result = edgeToNode.get(edgeID);
        if (result == null)
        {
            return -1;
        }
        return result;
    }

    public int getEdgeForNode(int nodeID)
    {
        Integer result = nodeToEdge.get(nodeID);
        if (result == null)
        {
            return -1;
        }
        return result;
    }
    
    public QTypeStep get(int id)
    {
        return retrieve(QTypeStep.class, id);
    }
}
