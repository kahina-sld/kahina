package org.kahina.gui.event;

import org.kahina.control.event.KahinaEvent;

public class KahinaUpdateEvent extends KahinaEvent
{
    int selectedStep;
    
    public KahinaUpdateEvent(int selectedStep)
    {
        super("update");
        this.selectedStep = selectedStep;
    }
    
    public int getSelectedStep()
    {
        return selectedStep;
    }
}
