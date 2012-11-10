package org.kahina.core.control;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import org.kahina.core.profiler.KahinaLogger;

/**
 * The central broker for event handling and passing of messages.
 * <p>
 * Basic concept: 
 * - an object can fire events via <code>processEvent</code> if it has access to a KahinaController instance
 * - KahinaListeners can register themselves as listeners for event types
 * - the controller passes each fired event to all KahinaListeners that have registered for its type
 */

public class KahinaController
{
	private static final boolean VERBOSE = false;
	
    //map from types to interested listeners
    private HashMap<String,List<KahinaListener>> registry;
    
    //used for printing out indented processing statistics
    private int currentEventDepth = 0;
    
    KahinaLogger logger;
    
    public KahinaController(KahinaLogger logger)
    {
        registry = new HashMap<String,List<KahinaListener>>();
        this.logger = logger;
    }
    
    public void registerListener(String type, KahinaListener listener)
    {
        if (VERBOSE)
        {
            System.err.println("KahinaController@" + this.hashCode() + ".registerListener(" + type + ", " + listener.getClass().getSimpleName() + "@" + listener.hashCode() + ")");
        }
        List<KahinaListener> listenersForType = registry.get(type);
        if (listenersForType == null)
        {
            listenersForType = new LinkedList<KahinaListener>();
            registry.put(type, listenersForType);
        } 
        else if (listenersForType.contains(listener))
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
        if (VERBOSE)
        {
        	System.err.println("KahinaController@" + this.hashCode() + ".processEvent(" + event + "@" + event.hashCode() + ")");
        }
        logger.startMeasuring();
        String type = event.getType();
        List<KahinaListener> listenersForType = registry.get(type);
        if (listenersForType != null)
        {
            for (int i = 0; i < listenersForType.size(); i++)
            {
            	if (VERBOSE)
            	{
            		System.err.println("    event \"" + event + "@" + event.hashCode() + "\" to listener " + listenersForType.get(i).getClass().getSimpleName() + "@" + listenersForType.get(i).hashCode());
            	}
                listenersForType.get(i).processEvent(event);
            }
        }
        logger.endMeasuring("for processing event\"" + event + "\" in " + "KahinaController@" + this.hashCode());
    }
}
