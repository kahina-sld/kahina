package org.kahina.core.bridge;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.event.KahinaControlEvent;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.gui.KahinaGUI;

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
    
    //method stub to prevent infinite recursion; implemented by specialized bridges
    public void processEvent(KahinaControlEvent e)
    {
        
    }
}
