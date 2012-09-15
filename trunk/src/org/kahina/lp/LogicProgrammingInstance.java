package org.kahina.lp;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.data.breakpoint.KahinaControlPoint;
import org.kahina.core.data.breakpoint.KahinaControlPointProfile;
import org.kahina.core.data.project.KahinaProject;
import org.kahina.lp.behavior.LogicProgrammingTreeBehavior;
import org.kahina.lp.bridge.LogicProgrammingBridge;
import org.kahina.lp.control.NewControlAgentEvent;
import org.kahina.lp.data.project.LogicProgrammingProject;
import org.kahina.lp.gui.LogicProgrammingGUI;
import org.kahina.lp.profiler.LogicProgrammingProfiler;

public abstract class LogicProgrammingInstance<S extends LogicProgrammingState, G extends LogicProgrammingGUI, B extends LogicProgrammingBridge, P extends LogicProgrammingProject> extends KahinaInstance<S, G, B, P>
{   
	public abstract LogicProgrammingProfiler getProfiler();
    
    public S getState()
    {
        return state;
    }
	
	@Override
	protected void createTreeBehavior()
	{
		new LogicProgrammingTreeBehavior(state.getStepTree(), this, state.getSecondaryStepTree());	
	}
	
    public KahinaControlPointProfile getBreakPoints()
    {
        if (project == null) return null;
        return project.getBreakPoints();
    }
    
    public KahinaControlPointProfile getCreepPoints()
    {
        if (project == null) return null;
        return project.getCreepPoints();
    }
    
    public KahinaControlPointProfile getCompletePoints()
    {
        if (project == null) return null;
        return project.getCompletePoints();
    }
    
    public KahinaControlPointProfile getSkipPoints()
    {
        if (project == null) return null;
        return project.getSkipPoints();
    }
    
    public KahinaControlPointProfile getFailPoints()
    {
        if (project == null) return null;
        return project.getFailPoints();
    }
    
    public KahinaControlPointProfile getWarnPoints()
    {
        if (project == null) return null;
        return project.getBreakPoints();
    }
    
    public void processEvent(KahinaEvent event)
    {
        super.processEvent(event);
        if (event instanceof NewControlAgentEvent)
        {
            processNewAgentEvent((NewControlAgentEvent) event);
        }
    }
    
    private void processNewAgentEvent(NewControlAgentEvent event)
    {
        KahinaControlPoint controlAgent = event.getControlAgent();
        switch (event.getAgentType())
        {
            case BREAK_AGENT:
            {
                project.addBreakPoint(controlAgent);
                break;
            }
            case CREEP_AGENT:
            {
                project.addCreepPoint(controlAgent);
                break;
            }
            case COMPLETE_AGENT:
            {
                project.addCompletePoint(controlAgent);
                break;
            }
            case SKIP_AGENT:
            {
                project.addSkipPoint(controlAgent);
                break;
            }
            case FAIL_AGENT:
            {
                project.addFailPoint(controlAgent);
                break;
            }
        }
    }
}
