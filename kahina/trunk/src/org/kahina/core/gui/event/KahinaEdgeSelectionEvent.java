package org.kahina.core.gui.event;

import org.kahina.core.event.KahinaEvent;
import org.kahina.core.visual.chart.KahinaChartViewPanel;

/**
 * Event to indicate a certain chart edge has been "marked" in a chart
 * and all interested parties should be updated accordingly.
 * @author jd
 *
 */
public class KahinaEdgeSelectionEvent extends KahinaEvent
{
    int selectedEdge;
    KahinaChartViewPanel panel;
    
    public KahinaEdgeSelectionEvent(int selectedEdge)
    {
        super("edge select");
        this.selectedEdge = selectedEdge;
        this.panel = null;
    }
    
    public KahinaEdgeSelectionEvent(int selectedEdge, KahinaChartViewPanel panel)
    {
        super("select");
        this.selectedEdge = selectedEdge;
        this.panel = panel;
    }
    
    public int getSelectedEdge()
    {
        return selectedEdge;
    }
    
    public KahinaChartViewPanel getPanel()
    {
        return panel;
    }
    
    public String toString()
    {
        return  "select edge " + selectedEdge;
    }
}
