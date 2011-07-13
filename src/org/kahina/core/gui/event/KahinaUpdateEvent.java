package org.kahina.core.gui.event;

import org.kahina.core.event.KahinaEvent;
import org.kahina.core.event.KahinaEventTypes;

public class KahinaUpdateEvent extends KahinaEvent
{
    int selectedStep;
    
    public KahinaUpdateEvent(int selectedStep)
    {
        super(KahinaEventTypes.UPDATE);
        this.selectedStep = selectedStep;
    }
    
    public int getSelectedStep()
    {
        return selectedStep;
    }
    
    @Override
	public String toString()
    {
        return  "update: node " + selectedStep;
    }
}
