package org.kahina.core.gui.event;

import org.kahina.core.event.KahinaEvent;

public class KahinaChartUpdateEvent extends KahinaEvent
{
    int selectedEdge;
    
    public KahinaChartUpdateEvent(int selectedEdge)
    {
        super("chart update");
        this.selectedEdge = selectedEdge;
    }
    
    public int getSelectedEdge()
    {
        return selectedEdge;
    }
    
    public String toString()
    {
        return "update: edge " + selectedEdge;
    }
}
