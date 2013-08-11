package org.kahina.swi;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.LinkedList;

import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.core.gui.KahinaViewRegistry;
import org.kahina.core.io.util.XMLUtil;
import org.kahina.lp.LogicProgrammingInstance;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.lp.data.project.LogicProgrammingProject;
import org.kahina.lp.profiler.LogicProgrammingProfiler;
import org.kahina.lp.visual.source.PrologJEditSourceCodeView;
import org.kahina.prolog.profiler.PrologProfiler;
import org.kahina.swi.bridge.SWIPrologBridge;
import org.kahina.swi.data.bindings.SWIPrologVariableBindingSet;
import org.kahina.swi.gui.SWIPrologGUI;
import org.kahina.swi.visual.bindings.SWIPrologVariableBindingSetView;
import org.w3c.dom.Document;

public class SWIPrologDebuggerInstance extends LogicProgrammingInstance<LogicProgrammingState, SWIPrologGUI, SWIPrologBridge, LogicProgrammingProject>
{

	PrologProfiler profiler;

	@Override
	public SWIPrologBridge startNewSession()
	{
		try
		{
			super.startNewSession();
			profiler = new PrologProfiler(this, state.getFullProfile());
			return bridge;
		} 
        catch (Exception e)
		{
			e.printStackTrace();
			System.exit(-1);
		}
		return null;
	}

	@Override
	public LogicProgrammingProfiler getProfiler()
	{
		return profiler;
	}

	@Override
	protected SWIPrologBridge createBridge()
	{
		return new SWIPrologBridge(this);
	}

	@Override
	protected SWIPrologGUI createGUI()
	{
		return new SWIPrologGUI(SWIPrologStep.class, this);
	}

	@Override
	protected LogicProgrammingState createState()
	{
		return new LogicProgrammingState(this);
	}
	
	@Override
	protected void fillViewRegistry()
	{
		super.fillViewRegistry();
		KahinaViewRegistry.registerMapping(SWIPrologVariableBindingSet.class, SWIPrologVariableBindingSetView.class);
		KahinaViewRegistry.registerMapping(KahinaSourceCodeLocation.class, PrologJEditSourceCodeView.class);
	}
	
	public static void main(String[] args)
	{
		(new SWIPrologDebuggerInstance()).start(args);
	}

    @Override
    protected LogicProgrammingProject createNewProject()
    {
        return new LogicProgrammingProject("swi-prolog", "no name", this);
    }
    
    @Override
    public LogicProgrammingProject loadProject(File file)
    {
    	InputStream stream;
		try
		{
			stream = new FileInputStream(file);
		} catch (FileNotFoundException e)
		{
			System.err.println("ERROR: Project file not found!");
			e.printStackTrace();
			return null;
		}
        Document dom;
        LogicProgrammingProject project = createNewProject();
        dom = XMLUtil.parseXMLStream(stream, false);
        LogicProgrammingProject.importXML(dom.getDocumentElement(), project, this, state.getStepTree());
        return project;
    }
    
    @Override
    protected void prepareProjectLists()
    {
        recentProjects = new LinkedList<LogicProgrammingProject>();
        defaultProjects = new LinkedList<LogicProgrammingProject>();
    }

    @Override
    protected void preparePerspectiveLists()
    {
        recentPerspectives = new LinkedList<KahinaPerspective>();
        defaultPerspectives = new LinkedList<KahinaPerspective>();    
    } 

}
