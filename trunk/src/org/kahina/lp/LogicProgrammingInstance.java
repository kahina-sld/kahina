package org.kahina.lp;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.agent.KahinaControlAgent;
import org.kahina.core.data.agent.KahinaControlAgentProfile;
import org.kahina.core.data.project.KahinaProject;
import org.kahina.core.data.project.KahinaProjectStatus;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.gui.KahinaViewRegistry;
import org.kahina.core.io.util.XMLUtil;
import org.kahina.core.visual.KahinaDefaultView;
import org.kahina.core.visual.source.KahinaJEditSourceCodeView;
import org.kahina.core.visual.tree.KahinaTreeView;
import org.kahina.lp.behavior.LogicProgrammingTreeBehavior;
import org.kahina.lp.bridge.LogicProgrammingBridge;
import org.kahina.lp.control.NewControlAgentEvent;
import org.kahina.lp.data.project.LogicProgrammingProject;
import org.kahina.lp.gui.LogicProgrammingGUI;
import org.kahina.lp.profiler.LogicProgrammingProfiler;
import org.w3c.dom.Document;

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
	
    public KahinaControlAgentProfile getBreakPoints()
    {
        if (project == null) return null;
        return project.getBreakPoints();
    }
    
    public KahinaControlAgentProfile getCreepPoints()
    {
        if (project == null) return null;
        return project.getCreepPoints();
    }
    
    public KahinaControlAgentProfile getCompletePoints()
    {
        if (project == null) return null;
        return project.getCompletePoints();
    }
    
    public KahinaControlAgentProfile getSkipPoints()
    {
        if (project == null) return null;
        return project.getSkipPoints();
    }
    
    public KahinaControlAgentProfile getFailPoints()
    {
        if (project == null) return null;
        return project.getFailPoints();
    }
    
    public KahinaControlAgentProfile getWarnPoints()
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
        KahinaControlAgent controlAgent = event.getControlAgent();
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
    
    public void newProject(File grammarFile)
    {
        project = createNewProject();
        project.setMainFile(grammarFile);
        project.setPerspective(gui.getPerspective());
        setProjectStatus(KahinaProjectStatus.PROGRAM_UNCOMPILED);
        gui.displayMainViews();
    }
    
    public P loadProject(InputStream stream)
    {
        System.err.println("LogicProgrammingInstance.loadProject");
        Document dom;
        P project = createNewProject();
        dom = XMLUtil.parseXMLStream(stream, false);
        LogicProgrammingProject.importXML(dom.getDocumentElement(), project);
        return project;
    }
    
    protected void fillViewRegistry()
    {
        super.fillViewRegistry();
        KahinaViewRegistry.registerMapping(KahinaTree.class, KahinaTreeView.class);
        KahinaViewRegistry.registerMapping(KahinaSourceCodeLocation.class, KahinaJEditSourceCodeView.class);
    }
}
