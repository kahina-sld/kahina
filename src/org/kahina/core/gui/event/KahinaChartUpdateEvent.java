package org.kahina.core.gui.event;

import org.kahina.core.control.KahinaEvent;

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
    
    @Override
	public String toString()
    {
        return "update: edge " + selectedEdge;
    }
}
