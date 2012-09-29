package org.kahina.sicstus;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.LinkedList;

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
		return new LogicProgrammingState(this);
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
        return new LogicProgrammingProject("sicstus", "no name", state.getStepTree(), this);
    }

    public LogicProgrammingProject loadProject(File projectFile)
    {
        Document dom;
        LogicProgrammingProject project = createNewProject();
        try
        {
            dom = XMLUtil.parseXMLStream(new FileInputStream(projectFile), false);
            project = LogicProgrammingProject.importXML(dom.getDocumentElement(), project, this, state.getStepTree());
        }
        catch (FileNotFoundException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return project;
    }
    
    @Override
    protected void prepareProjectLists()
    {
        recentProjects = new LinkedList<LogicProgrammingProject>();
        // load the default perspectives in the bin folder of the respective Kahina application
        defaultProjects = new LinkedList<LogicProgrammingProject>();
        // This filter only returns XML files
        FileFilter fileFilter = new FileFilter()
        {
            public boolean accept(File file)
            {
                // System.err.println("Filtering file " + file.getName() + ": "
                // + file.getName().endsWith("xml"));
                return file.getName().endsWith("xml");
            }
        };
        File[] files = new File(LogicProgrammingInstance.class.getResource("./data/project").getFile()).listFiles(fileFilter);
        for (File f : files)
        {
            if (VERBOSE)
            {
                System.err.println("Loading predefined project: " + f.getAbsolutePath());
            }
            defaultProjects.add(loadProject(f));
        }
        
    } 
}
