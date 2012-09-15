package org.kahina.swi;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

import org.kahina.core.data.project.KahinaProject;
import org.kahina.core.data.project.KahinaProjectStatus;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
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
		return new LogicProgrammingState(control);
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
        return new LogicProgrammingProject("swi-prolog", state.getStepTree(), control);
    }
    
    public void loadProject(File projectFile)
    {
        Document dom;
        try
        {
            dom = XMLUtil.parseXMLStream(new FileInputStream(projectFile), false);
            project = createNewProject();
            project = LogicProgrammingProject.importXML(dom.getDocumentElement(), project);
            setProjectStatus(KahinaProjectStatus.PROGRAM_UNCOMPILED);
        }
        catch (FileNotFoundException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

}
