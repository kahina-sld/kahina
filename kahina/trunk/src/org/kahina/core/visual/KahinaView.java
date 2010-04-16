package org.kahina.core.visual;

import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.event.KahinaEvent;

public class KahinaView<T extends KahinaObject> implements KahinaListener
{
    //handle global events by means of a controller
    protected KahinaController control;
    
    //the title of the tab or window this view is displayed in
    private String title = "Unnamed View";
    
    protected T model;
    
    public void processEvent(KahinaEvent event)
    {
        System.err.println("KahinaView received event: " + event.toString());
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
    
    public KahinaViewPanel<T> wrapInPanel()
    {
        KahinaViewPanel<T> panel = new KahinaViewPanel<T>();
        panel.setView(this);
        return panel;
    }
    
    public String getTitle()
    {
        return title;
    }
    
    public void setTitle(String title)
    {
        this.title = title;
    }
}
