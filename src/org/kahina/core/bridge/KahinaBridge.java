package org.kahina.core.bridge;

import org.kahina.core.KahinaRunner;
import org.kahina.core.KahinaStep;
import org.kahina.core.control.KahinaBreakpoint;
import org.kahina.core.control.KahinaBreakpointType;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.event.KahinaControlEvent;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.event.KahinaEventTypes;
import org.kahina.core.event.KahinaSystemEvent;
import org.kahina.core.event.KahinaTreeMatchEvent;
import org.kahina.core.event.KahinaWarnEvent;
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
    
    public KahinaBridge()
    {
        KahinaRunner.getControl().registerListener(KahinaEventTypes.CONTROL, this);
        KahinaRunner.getControl().registerListener(KahinaEventTypes.TREE_MATCH, this);
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
    	} else if (e instanceof KahinaControlEvent)
        {
            processControlEvent((KahinaControlEvent) e);
        } else if (e instanceof KahinaSystemEvent)
        {
        	processSystemEvent((KahinaSystemEvent) e);
        }
        else if (e instanceof KahinaTreeMatchEvent)
        {
            processEvent((KahinaTreeMatchEvent) e);
        } else if (e instanceof KahinaWarnEvent)
        {
        	processWarnEvent((KahinaWarnEvent) e);
        }
    }
    
    //method stub to prevent infinite recursion; implemented by specialized bridges
    protected void processControlEvent(KahinaControlEvent e)
    {
        
    }
    
    protected void processSelectionEvent(KahinaSelectionEvent e)
    {
    	
    }
    
    protected void processEvent(KahinaTreeMatchEvent e)
    {
    	if (VERBOSE)
    	{
    		System.err.println(this + ".processEvent(" + e + ")");
    	}
        switch (e.getBreakpoint().getType())
        {
            case KahinaBreakpointType.SKIP_POINT:
            {
            	if (VERBOSE)
            	{
            		System.err.println("It's a skip point!");
            	}
                processSkipPointMatch(e.getNodeID(), e.getBreakpoint());
                break;
            }
            case KahinaBreakpointType.CREEP_POINT:
            {
            	if (VERBOSE)
            	{
            		System.err.println("It's a creep point!");
            	}
                processCreepPointMatch(e.getNodeID(), e.getBreakpoint());
                break;
            }
            case KahinaBreakpointType.FAIL_POINT:
            {
                processFailPointMatch(e.getNodeID(), e.getBreakpoint());
                break;
            }
            case KahinaBreakpointType.PRIMARY_BREAKPOINT:
            {
                //no break here, same as next case
            }
            case KahinaBreakpointType.SECONDARY_BREAKPOINT:
            {
                processBreakPointMatch(e.getNodeID(), e.getBreakpoint());
                break;
            }
            case KahinaBreakpointType.PROFILE_POINT:
            {
                processProfilePointMatch(e.getNodeID(), e.getBreakpoint());
                break;
            }
        }
    }
    
    protected void processSkipPointMatch(int nodeID, KahinaBreakpoint bp)
    {
        
    }
    
    protected void processCreepPointMatch(int nodeID, KahinaBreakpoint bp)
    {
        
    }
    
    protected void processFailPointMatch(int nodeID, KahinaBreakpoint bp)
    {
        
    }
    
    protected void processBreakPointMatch(int nodeID, KahinaBreakpoint bp)
    {
        
    }
    
    protected void processProfilePointMatch(int nodeID, KahinaBreakpoint bp)
    {
        
    }

	protected void processSystemEvent(KahinaSystemEvent e)
	{
	}
	
	protected void processWarnEvent(KahinaWarnEvent e)
	{
	}
}
