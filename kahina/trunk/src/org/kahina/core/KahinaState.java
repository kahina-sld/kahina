package org.kahina.core;

import java.io.Serializable;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.text.KahinaLineReference;
import org.kahina.core.data.text.KahinaTextModel;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.event.KahinaMessageEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;

/**
 * The current state of a Kahina instance.
 * 
 * Implicitly contains all the information on the current run of Kahina.
 * 
 * Can be saved to and loaded from a database file, allowing to interrupt and continue runs.
 * 
 *  @author jdellert
 */

public class KahinaState implements Serializable, KahinaListener
{   
    /**
	 * 
	 */
	private static final long serialVersionUID = -1884751676781509811L;
	//the messages that will be stored in the console
    protected KahinaTextModel consoleMessages;
    //map from stepIDs to lines in console
    protected Map<Integer,Set<KahinaLineReference>> consoleLines;
    
    private int selectedStepID = -1;
    
    public KahinaState()
    {
        consoleMessages = new KahinaTextModel();
        consoleLines = new HashMap<Integer,Set<KahinaLineReference>>();
        KahinaRunner.getControl().registerListener("select", this);
    }
    
    public void processEvent(KahinaEvent event)
    {
    	if (event instanceof KahinaSelectionEvent)
    	{
    		processSelectionEvent((KahinaSelectionEvent) event);
    	}
    }
    
    private void processSelectionEvent(KahinaSelectionEvent event)
	{
		selectedStepID = event.getSelectedStep();
	}
    
    public int getSelectedStepID()
    {
    	return selectedStepID;
    }

	public void consoleMessage(int stepID, String message)
    {
        int lineID = consoleMessages.text.addLine(message);
        KahinaLineReference ref = new KahinaLineReference(consoleMessages,lineID,stepID);
        Set<KahinaLineReference> refs = consoleLines.get(stepID);
        if (refs == null)
        {
            refs = new HashSet<KahinaLineReference>();
            consoleLines.put(stepID, refs);
        }
        refs.add(ref);
        KahinaRunner.processEvent(new KahinaMessageEvent(ref));
    }
    
    public KahinaTextModel getConsoleMessages()
    {
        return consoleMessages;
    }
    
    public Set<KahinaLineReference> getLineReferencesForStep(int stepID)
    {
    	return consoleLines.get(stepID);
    }
}
