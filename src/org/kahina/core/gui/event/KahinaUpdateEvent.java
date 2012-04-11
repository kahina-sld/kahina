package org.kahina.core.gui.event;

import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;

public class KahinaUpdateEvent extends KahinaEvent
{
    private final int selectedStep;
    
    private final int layer;
    
    public KahinaUpdateEvent(int selectedStep, int layer)
    {
        super(KahinaEventTypes.UPDATE);
        this.selectedStep = selectedStep;
        this.layer = layer;
    }
    
    public int getSelectedStep()
    {
        return selectedStep;
    }
    
    public int getLayer()
    {
    	return layer;
    }
    
    @Override
	public String toString()
    {
        return  "update: node " + selectedStep;
    }
}
