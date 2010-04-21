package org.kahina.core.visual;

import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.event.KahinaEvent;

public abstract class KahinaView<T extends KahinaObject> implements KahinaListener
{  
    //the title of the tab or window this view is displayed in
    private String title = "Unnamed View";
    
    protected T model;
    
    public void processEvent(KahinaEvent event)
    {
        System.err.println(this + " received event: " + event.toString());
        recalculate();
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
    
    public abstract KahinaViewPanel<?> wrapInPanel();
    
    public String getTitle()
    {
        return title;
    }
    
    public void setTitle(String title)
    {
        this.title = title;
    }
}
