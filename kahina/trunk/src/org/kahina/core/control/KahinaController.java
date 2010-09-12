package org.kahina.core.control;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import org.kahina.core.event.KahinaEvent;

/**
 * the central broker for event handling and passing of messages
 * 
 * basic concept: 
 * - an object can fire events via <code>processEvent</code> if it has access to a KahinaController instance
 * - KahinaListeners can register themselves as listeners for event types
 * - the controller passes each fired event to all KahinaListeners that have registered for its type
 */

public class KahinaController
{
	private static final boolean verbose = false;
	
    //map from types to interested listeners
    HashMap<String,List<KahinaListener>> registry;
    
    public KahinaController()
    {
        registry = new HashMap<String,List<KahinaListener>>();
    }
    
    public void registerListener(String type, KahinaListener listener)
    {
        List<KahinaListener> listenersForType = registry.get(type);
        if (listenersForType == null)
        {
            listenersForType = new LinkedList<KahinaListener>();
            registry.put(type, listenersForType);
        } else if (listenersForType.contains(listener))
        {
        	return;
        }
        listenersForType.add(listener);
    }
    
    /**
     * 
     * @param type
     * @param listener
     * @return <code>true</code> if listener was successfully removed, <code>false</code> if it never existed
     */
    public boolean removeListener(String type, KahinaListener listener)
    {
        List<KahinaListener> listenersForType = registry.get(type);
        if (listenersForType == null) return false;
        return listenersForType.remove(listener);
    }
    
    public void processEvent(KahinaEvent event)
    {
        if (verbose)
        {
        	System.err.println("Controller processing event: " + event);
        }
        String type = event.getType();
        List<KahinaListener> listenersForType = registry.get(type);
        if (listenersForType != null)
        {
            for (int i = 0; i < listenersForType.size(); i++)
            {
                listenersForType.get(i).processEvent(event);
            }
        }
    }
}
