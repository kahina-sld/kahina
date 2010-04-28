package org.kahina.core.visual;

import javax.swing.JComponent;

import org.kahina.core.KahinaRunner;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;

public abstract class KahinaView<T extends KahinaObject> implements KahinaListener
{  
    //the title of the tab or window this view is displayed in
    private String title = "Unnamed View";
    
    protected T model;
    
    public void processEvent(KahinaEvent e)
    {
        //System.err.println(this + " received event: " + e.toString());
        if (e instanceof KahinaUpdateEvent)
        {
            processEvent((KahinaUpdateEvent) e);
        }
        else if (e instanceof KahinaSelectionEvent)
        {
            processEvent((KahinaSelectionEvent) e);
        }
    }
    
    public void processEvent(KahinaUpdateEvent e)
    {
        recalculate();
    }
    
    public void processEvent(KahinaSelectionEvent e)
    {
        KahinaRunner.processEvent(new KahinaUpdateEvent(e.getSelectedStep()));
        KahinaRunner.processEvent(new KahinaRedrawEvent());
    }
    
    public void display(T model)
    {
        this.model = model;
    }
    
    public T getModel()
    {
        return model;
    }
    
    //override this method to define necessary operations after changes to the model (coordinate recomputations etc.)
    protected void recalculate()
    {
        
    }
    
    public abstract JComponent wrapInPanel();
    
    public String getTitle()
    {
        return title;
    }
    
    public void setTitle(String title)
    {
        this.title = title;
    }
}
