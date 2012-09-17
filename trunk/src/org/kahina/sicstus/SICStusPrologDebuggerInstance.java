package org.kahina.sicstus;

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
import org.kahina.sicstus.bridge.SICStusPrologBridge;
import org.kahina.sicstus.data.bindings.SICStusPrologVariableBindingSet;
import org.kahina.sicstus.gui.SICStusPrologGUI;
import org.kahina.sicstus.visual.bindings.SICStusPrologVariableBindingSetView;
import org.w3c.dom.Document;

public class SICStusPrologDebuggerInstance extends LogicProgrammingInstance<LogicProgrammingState, SICStusPrologGUI, SICStusPrologBridge, LogicProgrammingProject>
{

	PrologProfiler profiler;

	@Override
	public SICStusPrologBridge startNewSession()
	{
		try
		{
			super.startNewSession();
			profiler = new PrologProfiler(this, state.getFullProfile());
			return bridge;
		} catch (Exception e)
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
	protected SICStusPrologBridge createBridge()
	{
		return new SICStusPrologBridge(this);
	}

	@Override
	protected SICStusPrologGUI createGUI()
	{
		return new SICStusPrologGUI(SICStusPrologStep.class, this);
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
		KahinaViewRegistry.registerMapping(SICStusPrologVariableBindingSet.class, SICStusPrologVariableBindingSetView.class);
		KahinaViewRegistry.registerMapping(KahinaSourceCodeLocation.class, PrologJEditSourceCodeView.class);
	}

	public static void main(String[] args)
	{
		(new SICStusPrologDebuggerInstance()).start(args);
	}

    @Override
    protected LogicProgrammingProject createNewProject()
    {
        return new LogicProgrammingProject("sicstus", state.getStepTree(), control);
    }

    public void loadProject(File projectFile)
    {
        Document dom;
        try
        {
            dom = XMLUtil.parseXMLStream(new FileInputStream(projectFile), false);
            project = createNewProject();
            project = LogicProgrammingProject.importXML(dom.getDocumentElement(), project, control);
            setProjectStatus(KahinaProjectStatus.PROGRAM_UNCOMPILED);
        }
        catch (FileNotFoundException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
}
