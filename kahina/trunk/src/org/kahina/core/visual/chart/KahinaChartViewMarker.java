package org.kahina.core.visual.chart;

import org.kahina.core.KahinaRunner;
import org.kahina.core.data.chart.KahinaChart;
import org.kahina.core.gui.event.KahinaSelectionEvent;

public class KahinaChartViewMarker
{
    int markedEdge = -1;
    KahinaChart view;
    
    public KahinaChartViewMarker(KahinaChart view)
    {
        this.view = view;
    }
 
    public void markEdge(int edgeID)
    {
        if (edgeID == -1)
        {
            markedEdge = edgeID;
            //view.setMarkedNode(-1);
            //view.updateDisplay();
            //view.repaint();
        }
        //KahinaRunner.processEvent(new KahinaSelectionEvent());
    }
}
