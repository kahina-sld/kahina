package org.kahina.core.bridge;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaStep;
import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.control.KahinaSystemEvent;
import org.kahina.core.control.KahinaWarnEvent;
import org.kahina.core.data.breakpoint.KahinaBreakpoint;
import org.kahina.core.data.breakpoint.KahinaBreakpointType;
import org.kahina.core.data.breakpoint.patterns.KahinaTreeMatchEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;

/**
 * policy: bridges may only operate directly on steps, not on complex structures (behaviors for that purpose)
 *
 * @author jdellert
 *
 */
public class KahinaBridge implements KahinaListener
{
	private static final boolean VERBOSE = false;
	protected KahinaInstance<?,?,?,?> kahina;
    
    public KahinaBridge(KahinaInstance<?,?,?,?> kahina)
    {
        if (kahina.getState() == null)
        {
            System.err.println("WARNING: attempting to construct a bridge for an instance where state == null!");
            System.err.println("         Bridge will not be functional! NullPointerExceptions might result!");
        }
    	this.kahina = kahina;
        kahina.getControl().registerListener(KahinaEventTypes.CONTROL, this);
        kahina.getControl().registerListener(KahinaEventTypes.TREE_MATCH, this);
    }
    
    protected KahinaStep generateStep()
    {
    	if (VERBOSE) System.err.println("KahinaBridge.generateStep()");
        return new KahinaStep();
    }
    
    public void processEvent(KahinaEvent e)
    {
    	if (e instanceof KahinaSelectionEvent)
    	{
    		processSelectionEvent((KahinaSelectionEvent) e);
    	} 
        else if (e instanceof KahinaControlEvent)
        {
            processControlEvent((KahinaControlEvent) e);
        } 
        else if (e instanceof KahinaSystemEvent)
        {
        	processSystemEvent((KahinaSystemEvent) e);
        }
        else if (e instanceof KahinaTreeMatchEvent)
        {
            processEvent((KahinaTreeMatchEvent) e);
        } 
        else if (e instanceof KahinaWarnEvent)
        {
        	processWarnEvent((KahinaWarnEvent) e);
        }
    }
    
    //method stub to prevent infinite recursion; implemented by specialized bridges
    protected void processControlEvent(KahinaControlEvent e)
    {
        System.err.println("WARNING: control event \"" + e + "\" ignored by default implementation in KahinaBridge!");
    }
    
    protected void processSelectionEvent(KahinaSelectionEvent e)
    {
    	
    }
    
	protected void processSystemEvent(KahinaSystemEvent e)
	{
	}
	
	protected void processWarnEvent(KahinaWarnEvent e)
	{
	}
}
