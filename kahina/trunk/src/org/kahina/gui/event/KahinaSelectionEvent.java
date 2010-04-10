package org.kahina.gui.event;

import org.kahina.control.event.KahinaEvent;
import org.kahina.visual.KahinaView;

public class KahinaSelectionEvent extends KahinaEvent
{
    int selectedStep;
    KahinaView view;
    
    public KahinaSelectionEvent(int selectedStep, KahinaView view)
    {
        super("select");
        this.selectedStep = selectedStep;
        this.view = view;
    }
    
    public int getSelectedStep()
    {
        return selectedStep;
    }
    
    public KahinaView getView()
    {
        return view;
    }
}
