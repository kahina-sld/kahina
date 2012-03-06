package org.kahina.tralesld.behavior;

import java.util.List;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaRunner;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.data.breakpoint.KahinaBreakpoint;
import org.kahina.core.data.breakpoint.KahinaBreakpointFactory;
import org.kahina.core.data.breakpoint.KahinaBreakpointType;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.lp.behavior.LogicProgrammingTreeBehavior;
import org.kahina.tralesld.TraleSLDStepStatus;
import org.kahina.tralesld.TraleSLDStepType;
import org.kahina.tralesld.bridge.TraleSLDBridgeEvent;
import org.kahina.tralesld.bridge.TraleSLDBridgeEventType;

/**
 * This is supposed to contain the logic by which TraleSLD handles its step trees.
 * 
 * @author johannes
 *
 */

public class TraleSLDTreeBehavior extends LogicProgrammingTreeBehavior
{   
	private static final boolean VERBOSE = false;
	
    public TraleSLDTreeBehavior(KahinaTree tree, KahinaInstance<?, ?, ?> kahina, KahinaTree secondaryTree)
    {
        super(tree, kahina, secondaryTree);  
        kahina.getControl().registerListener("traleSLD bridge", this);
    }
    
    @Override
	public void initializeSkipPoints()
    {
        List<KahinaBreakpoint> skipPoints = ((LogicProgrammingState) kahina.getState()).getSkipPoints();
        skipPoints.add(KahinaBreakpointFactory.createMatchingLabelBreakpoint("skip unification", "[0-9]* (unify|featval|type).*", KahinaBreakpointType.SKIP_POINT));
        skipPoints.add(KahinaBreakpointFactory.createMatchingLabelBreakpoint("skip compilation", "[0-9]* compile_gram.*", KahinaBreakpointType.SKIP_POINT));
    }
    
    @Override
	public void initializeCreepPoints()
    {
    	List<KahinaBreakpoint> creepPoints = ((LogicProgrammingState) kahina.getState()).getCreepPoints();
    	creepPoints.add(KahinaBreakpointFactory.createMatchingLabelBreakpoint("creep over lexical lookup", "[0-9]* lex\\(.*", KahinaBreakpointType.CREEP_POINT));
    	creepPoints.add(KahinaBreakpointFactory.createMatchingLabelBreakpoint("creep away from compilation", "[0-9]* compile_gram.*", KahinaBreakpointType.CREEP_POINT));
    }
    
    public void processRuleApplication(int stepID, int externalID, String ruleName)
    {
    	String caption = externalID + " rule(" + ruleName + ")";
        object.addNode(stepID, caption, "", TraleSLDStepStatus.PROGRESS);  
        secondaryTree.addNode(stepID, caption, "", TraleSLDStepStatus.PROGRESS);   
    }
    
    /**
     * registers and reacts to a finished step
     * @param stepID - the ID of the step that was finished in the monitored logic programming system
     */
    public void processStepFinished(int stepID)
    {
        if (VERBOSE) System.err.println("TraleSLDTreeBehavior.processStepFinished(" + stepID + ")");
        stepBeingRedone = -1;
        object.setNodeStatus(stepID,TraleSLDStepType.FINISHED);
        lastActiveID = object.getParent(stepID);
    }
    
    @Override
    public void processEvent(KahinaEvent e)
    {
        super.processEvent(e);
        if (e instanceof TraleSLDBridgeEvent)
        {
            processEvent((TraleSLDBridgeEvent) e);
        }
    }
    
    public void processEvent(TraleSLDBridgeEvent e)
    {
        switch (e.getEventType())
        {
            case TraleSLDBridgeEventType.RULE_APP:
            {
                processRuleApplication(e.getInternalID(), e.getIntContent(), e.getStrContent());
                break;
            }
            case TraleSLDBridgeEventType.STEP_FINISHED:
            {
                processStepFinished(e.getInternalID());
                break;
            }
        }
    }
}
