package org.kahina.core;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.kahina.core.breakpoint.KahinaBreakpoint;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.text.KahinaLineReference;
import org.kahina.core.data.text.KahinaTextModel;
import org.kahina.core.data.tree.KahinaMemTree;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.event.KahinaMessageEvent;
import org.kahina.core.gui.event.KahinaConsoleLineEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;

/**
 * The current state of a Kahina instance.
 * 
 * Implicitly contains all the information on the current run of Kahina.
 * 
 * Can be saved to and loaded from a database file, allowing to interrupt and continue runs.
 * 
 *  @author jdellert
 */

public class KahinaState implements KahinaListener
{
    KahinaTree stepTree;
    KahinaTree secondaryStepTree;
    
    // A kind of tertiary tree structure defined by links between nodes:
    Map<Integer, List<Integer>> anchorsByTarget;
    Map<Integer, Integer> targetByAnchor;
    
    //the messages that will be stored in the console
    protected KahinaTextModel consoleMessages;
    //map from stepIDs to lines in console
    protected Map<Integer,Set<KahinaLineReference>> consoleLines;
    
    //store the three types of breakpoints
    protected List<KahinaBreakpoint> primaryBreakpoints;
    protected List<KahinaBreakpoint> secondaryBreakpoints;
    protected List<KahinaBreakpoint> skipPoints;
    
    public KahinaState(KahinaInstance<?, ?, ?> kahina, int dataHandlingMethod)
    {
        stepTree = new KahinaMemTree();
        secondaryStepTree = new KahinaMemTree();
        anchorsByTarget = new HashMap<Integer, List<Integer>>();
        targetByAnchor = new HashMap<Integer, Integer>();
        consoleMessages = new KahinaTextModel();
        consoleLines = new HashMap<Integer,Set<KahinaLineReference>>();
        
        primaryBreakpoints = new ArrayList<KahinaBreakpoint>();
        secondaryBreakpoints = new ArrayList<KahinaBreakpoint>();
        skipPoints = new ArrayList<KahinaBreakpoint>();
        
        //database variant turned out to be too slow
        /* switch (dataHandlingMethod)
        {
            case KahinaDataHandlingMethod.DATABASE:
            {
                stepTree = new KahinaDbTree(KahinaRunner.getDatabaseHandler());
                secondaryStepTree = new KahinaDbTree(KahinaRunner.getDatabaseHandler());
                break;
            }
            case KahinaDataHandlingMethod.MEMORY:
            {
                stepTree = new KahinaMemTree();
                secondaryStepTree = new KahinaMemTree();
                break;
            }
        }*/
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
    
    public KahinaTree getStepTree()
    {
        return stepTree;
    }
    
    public KahinaTree getSecondaryStepTree()
    {
        return secondaryStepTree;
    }
    
    public List<KahinaBreakpoint> getPrimaryBreakpoints()
    {
        return primaryBreakpoints;
    }
    
    public List<KahinaBreakpoint> getSecondaryBreakpoints()
    {
        return secondaryBreakpoints;
    }
    
    public List<KahinaBreakpoint> getSkipPoints()
    {
        return skipPoints;
    }
    
    public void linkNodes(int anchor, int target)
    {
    	targetByAnchor.put(anchor, target);
    	List<Integer> anchors = anchorsByTarget.get(target);
    	if (anchors == null)
    	{
    		anchors = new ArrayList<Integer>();
    		anchorsByTarget.put(target, anchors);    		
    	}
		anchors.add(anchor);
    }
    
    public Integer getLinkTarget(int anchor)
    {
    	return targetByAnchor.get(anchor);
    }
    
    public List<Integer> getLinkAnchors(int target)
    {
    	List<Integer> result = anchorsByTarget.get(target);
    	if (result == null)
    	{
    		return Collections.emptyList();
    	}
    	return Collections.unmodifiableList(result);
    }
    
    public void processEvent(KahinaEvent e)
    {
        if (e instanceof KahinaUpdateEvent)
        {
            processEvent((KahinaUpdateEvent) e);
        }
    }
    
    public void processEvent(KahinaUpdateEvent e)
    {
        Set<KahinaLineReference> refs = consoleLines.get(e.getSelectedStep());
        if (refs != null) KahinaRunner.processEvent(new KahinaConsoleLineEvent(refs));
    }
}
