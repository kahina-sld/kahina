package org.kahina.bridge;

import org.kahina.control.KahinaController;
import org.kahina.control.KahinaListener;
import org.kahina.control.event.KahinaControlEvent;
import org.kahina.control.event.KahinaEvent;
import org.kahina.core.KahinaInstance;
import org.kahina.gui.KahinaGUI;

/**
 * policy: bridges may only operate directly on steps, not on complex structures (behaviors for that purpose)
 *
 * @author jdellert
 *
 */
public class KahinaBridge implements KahinaListener
{
    protected KahinaInstance kahina;
    protected KahinaGUI gui;
    protected KahinaController control;   
    
    public KahinaBridge(KahinaInstance kahina, KahinaGUI gui, KahinaController control)
    {
        this.kahina = kahina;
        this.gui = gui;
        this.control = control;
        control.registerListener("control", this);
    }
    
    public void processEvent(KahinaEvent e)
    {
        if (e instanceof KahinaControlEvent)
        {
            processEvent((KahinaControlEvent) e);
        }
    }
    
    //method stub to prevent infinite recursion
    public void processEvent(KahinaControlEvent e)
    {
        
    }
}
