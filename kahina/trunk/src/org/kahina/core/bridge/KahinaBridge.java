package org.kahina.core.bridge;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaRunner;
import org.kahina.core.KahinaStep;
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
	private static final boolean verbose = false;
	
    protected KahinaInstance<?, ?, ?> kahina;
    protected KahinaGUI gui; 
    
    public KahinaBridge(KahinaInstance<?, ?, ?> kahina, KahinaGUI gui)
    {
        this.kahina = kahina;
        this.gui = gui;
        KahinaRunner.getControl().registerListener("control", this);
    }
    
    public KahinaStep generateStep()
    {
    	if (verbose) System.err.println("KahinaBridge.generateStep()");
        return new KahinaStep();
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
