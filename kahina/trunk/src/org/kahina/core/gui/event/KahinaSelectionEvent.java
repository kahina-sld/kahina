package org.kahina.core.gui.event;

import org.kahina.core.event.KahinaEvent;
import org.kahina.core.visual.KahinaViewPanel;

/**
 * Event to indicate a certain step has been "marked" internally in the main
 * tree view and all views should be updated accordingly.
 * @author ke
 *
 */
public class KahinaSelectionEvent extends KahinaEvent
{
    int selectedStep;
    KahinaViewPanel<?> panel;
    
    public KahinaSelectionEvent(int selectedStep)
    {
        super("select");
        this.selectedStep = selectedStep;
        this.panel = null;
    }
    
    public KahinaSelectionEvent(int selectedStep, KahinaViewPanel<?> panel)
    {
        super("select");
        this.selectedStep = selectedStep;
        this.panel = panel;
    }
    
    public int getSelectedStep()
    {
        return selectedStep;
    }
    
    public KahinaViewPanel<?> getPanel()
    {
        return panel;
    }
    
    public String toString()
    {
        return  "select node " + selectedStep;
    }
}
