package org.kahina.lp;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaState;
import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.data.agent.KahinaBreakpoint;
import org.kahina.core.data.agent.KahinaControlAgent;
import org.kahina.core.data.agent.KahinaControlAgentProfile;
import org.kahina.core.data.text.KahinaLineReference;
import org.kahina.core.data.tree.KahinaMemTree;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.gui.event.KahinaMessageEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.lp.control.LogicProgrammingBreakActuator;
import org.kahina.lp.control.LogicProgrammingCompleteActuator;
import org.kahina.lp.control.LogicProgrammingCreepActuator;
import org.kahina.lp.control.LogicProgrammingFailActuator;
import org.kahina.lp.control.LogicProgrammingSkipActuator;
import org.kahina.lp.control.NewControlAgentEvent;
import org.kahina.lp.data.agent.LogicProgrammingControlAgentProfile;
import org.kahina.lp.data.text.LogicProgrammingLineReference;
import org.kahina.lp.profiler.LogicProgrammingProfile;

public class LogicProgrammingState extends KahinaState
{  
    /**
	 * 
	 */
	private static final long serialVersionUID = -2604998343572966299L;
	KahinaTree stepTree;
    KahinaTree secondaryStepTree;
    
    // A kind of tertiary tree structure defined by links between nodes:
    Map<Integer, List<Integer>> anchorsByTarget;
    Map<Integer, Integer> targetByAnchor;
    
    Set<Integer> hiddenSteps;
    
    protected LogicProgrammingProfile profile;
    
    public LogicProgrammingState(KahinaInstance<?,?,?,?> kahina)
    {
        super(kahina);
        stepTree = new KahinaMemTree();
        secondaryStepTree = new KahinaMemTree();
        anchorsByTarget = new HashMap<Integer, List<Integer>>();
        targetByAnchor = new HashMap<Integer, Integer>();
        hiddenSteps = new HashSet<Integer>();
        
        profile = new LogicProgrammingProfile();
    }
    
	public void initialize() 
	{
        super.initialize();
        stepTree = new KahinaMemTree();
        secondaryStepTree = new KahinaMemTree();
        anchorsByTarget = new HashMap<Integer, List<Integer>>();
        targetByAnchor = new HashMap<Integer, Integer>();
        hiddenSteps = new HashSet<Integer>();
        profile = new LogicProgrammingProfile();	
        //TODO: keep all breakpoints across parses
	}
    
    public void breakpointConsoleMessage(int stepID, String message)
    {
        int lineID = consoleMessages.text.addLine(message);
        KahinaLineReference ref = new LogicProgrammingLineReference(consoleMessages,lineID,stepID,-1, -1);
        kahina.dispatchInstanceEvent(new KahinaMessageEvent(ref));
    }
    
    public void consoleMessage(int stepID, int extID, int port, String message)
    {
        int lineID = consoleMessages.text.addLine(message);
        KahinaLineReference ref = new LogicProgrammingLineReference(consoleMessages,lineID,stepID,extID,port);
        Set<KahinaLineReference> refs = consoleLines.get(stepID);
        if (refs == null)
        {
            refs = new HashSet<KahinaLineReference>();
            consoleLines.put(stepID, refs);
        }
        refs.add(ref);
        kahina.dispatchInstanceEvent(new KahinaMessageEvent(ref));
    }
    
    public void exceptionConsoleMessage(int stepID, int extID, String message)
    {
    	int lineID = consoleMessages.text.addLine(message);
    	KahinaLineReference ref = new LogicProgrammingLineReference(consoleMessages, lineID, stepID, extID, LogicProgrammingStepType.EXCEPTION);
    	Set<KahinaLineReference> refs = consoleLines.get(stepID);
        if (refs == null)
        {
            refs = new HashSet<KahinaLineReference>();
            consoleLines.put(stepID, refs);
        }
        refs.add(ref);
        kahina.dispatchInstanceEvent(new KahinaMessageEvent(ref));
    }
    
    public void consoleMessage(LogicProgrammingLineReference ref)
    {
        Set<KahinaLineReference> refs = consoleLines.get(ref.step);
        if (refs == null)
        {
            refs = new HashSet<KahinaLineReference>();
            consoleLines.put(ref.step, refs);
        }
        refs.add(ref);
        //ref.store();
        kahina.dispatchInstanceEvent(new KahinaMessageEvent(ref));
    }
    
    public LogicProgrammingLineReference getConsoleLineRefForStep(int stepID)
    {
        Set<KahinaLineReference> refs = consoleLines.get(stepID);
        if (refs == null) return null;
        return (LogicProgrammingLineReference) refs.iterator().next();
    }
    
    public KahinaTree getStepTree()
    {
        return stepTree;
    }
    
    public KahinaTree getSecondaryStepTree()
    {
        return secondaryStepTree;
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

	public LogicProgrammingProfile getFullProfile()
	{
		return profile;
	}

	/**
	 * Calling this method indicates that a step should be hidden in the main
	 * tree view because it was skipped over and nevertheless reported by the
	 * logic programming system because it is "unskippable".
	 * @param stepID
	 */
	public void hideStep(int stepID)
	{
		hiddenSteps.add(stepID);
	}
	
	/**
	 * @return The IDs of the steps that should be hidden in the main tree view
	 * because they were skipped over and nevertheless reported by the logic
	 * programming system because they are "unskippable".
	 */
	public Set<Integer> getHiddenSteps()
	{
		return Collections.unmodifiableSet(hiddenSteps);
	}
	
    public LogicProgrammingStep get(int id)
    {
        return retrieve(LogicProgrammingStep.class, id);
    }
}
