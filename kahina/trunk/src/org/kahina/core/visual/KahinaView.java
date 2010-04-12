package org.kahina.core.visual;

import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.event.KahinaEvent;

public class KahinaView<T extends KahinaObject> implements KahinaListener
{
    //handle global events by means of a controller
    KahinaController control;
    
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
}
