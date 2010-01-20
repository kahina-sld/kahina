package org.kahina.visual;

import org.kahina.control.KahinaController;
import org.kahina.control.KahinaListener;
import org.kahina.control.event.KahinaEvent;

public class KahinaView implements KahinaListener
{
    //handle global events by means of a controller
    KahinaController control;
    
    public void processEvent(KahinaEvent event)
    {
        System.err.println("KahinaView recieved event: " + event.toString());
    }
}
