package org.kahina.core.gui.event;

import org.kahina.core.event.KahinaEvent;
import org.kahina.core.visual.KahinaView;

public class KahinaSelectionEvent extends KahinaEvent
{
    int selectedStep;
    KahinaView view;
    
    public KahinaSelectionEvent(int selectedStep)
    {
        super("select");
        this.selectedStep = selectedStep;
        this.view = null;
    }
    
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
    
    public String toString()
    {
        return  "select node " + selectedStep;
    }
}
