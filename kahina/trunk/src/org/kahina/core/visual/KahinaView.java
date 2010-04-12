package org.kahina.core.visual;

import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.event.KahinaEvent;

public class KahinaView<T extends KahinaObject> implements KahinaListener
{
    //handle global events by means of a controller
    KahinaController control;
    
    //the title of the tab or window this view is displayed in
    String title = "Unnamed View";
    
    protected T model;
    
    public void processEvent(KahinaEvent event)
    {
        System.err.println("KahinaView recieved event: " + event.toString());
    }
    
    public void display(T model)
    {
        
    }
    
    public T getModel()
    {
        return model;
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
