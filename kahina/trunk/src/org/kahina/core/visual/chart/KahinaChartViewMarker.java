package org.kahina.core.visual.chart;

import org.kahina.core.data.chart.KahinaChart;

public class KahinaChartViewMarker
{
    int markedEdge = -1;
    KahinaChart m;
    
    public KahinaChartViewMarker(KahinaChart m)
    {
        this.m = m;
    }
    
    //TODO: turn this into a test case for the future "tentacle" concept
    public void markEdge(int edgeID)
    {
        if (markedEdge != -1)
        {
            m.setEdgeStatus(markedEdge, m.getEdgeStatus(markedEdge) - 2);
        }
        if (edgeID != -1)
        {
            m.setEdgeStatus(edgeID, m.getEdgeStatus(edgeID) + 2);
        }
        markedEdge = edgeID;
    }
}
