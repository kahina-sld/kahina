package org.kahina.qtype;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.LinkedList;

import org.kahina.core.KahinaState;
import org.kahina.core.data.agent.KahinaBreakpointProfile;
import org.kahina.core.data.project.KahinaProject;
import org.kahina.core.data.project.KahinaProjectStatus;
import org.kahina.core.gui.KahinaViewRegistry;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.io.util.XMLUtil;
import org.kahina.lp.LogicProgrammingInstance;
import org.kahina.lp.behavior.LogicProgrammingTreeBehavior;
import org.kahina.lp.data.project.LogicProgrammingProject;
import org.kahina.qtype.bridge.QTypeBridge;
import org.kahina.qtype.data.bindings.QTypeGoal;
import org.kahina.qtype.data.project.QTypeProject;
import org.kahina.qtype.gui.QTypeGUI;
import org.kahina.qtype.visual.bindings.QTypeGoalView;
import org.kahina.sicstus.SICStusPrologDebuggerInstance;
import org.w3c.dom.Document;

public class QTypeDebuggerInstance extends SICStusPrologDebuggerInstance
{	
	
	private QTypeCommander commander;
	
	public QTypeDebuggerInstance()
	{
		super();
		commander = new QTypeCommander(this);
		commander.initializeForNewSession();
	}
	
	@Override
	public QTypeBridge startNewSession()
	{
		QTypeBridge bridge = (QTypeBridge) super.startNewSession();
		commander.initializeForNewSession();
		return bridge;
	}

	@Override
	protected QTypeGUI createGUI()
	{
		return new QTypeGUI(QTypeStep.class, this);
	}

	@Override
	protected void createTreeBehavior()
	{
		LogicProgrammingTreeBehavior behavior = new LogicProgrammingTreeBehavior(state.getStepTree(), this, state.getSecondaryStepTree());
		behavior.setMaxNodeLabelLength(-1);
	}

	@Override
	protected QTypeBridge createBridge()
	{
		return new QTypeBridge(this);
	}
	
	public static void main(String[] args)
	{
		(new QTypeDebuggerInstance()).start(args);
	}
	
	@Override
	protected void fillViewRegistry()
	{
		super.fillViewRegistry();
		KahinaViewRegistry.registerMapping(QTypeGoal.class, QTypeGoalView.class);
	}
	
	public String getCommand()
	{
		return commander.getCommand();
	}

	public QTypeCommander getCommander()
	{
		return commander;
	}
	
	@Override
	public String getApplicationName()
	{
		return "qtype";
	}
    
     @Override
    protected QTypeProject createNewProject()
    {
        return new QTypeProject("no name", state.getStepTree(), this);
    }
        
    public QTypeProject loadProject(File projectFile)
    {
        Document dom;
        QTypeProject project = createNewProject();
        try
        {
            dom = XMLUtil.parseXMLStream(new FileInputStream(projectFile), false);
            project = QTypeProject.importXML(dom.getDocumentElement(), project, this, state.getStepTree());
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
        File[] files = new File(this.getClass().getResource("./data/project").getFile()).listFiles(fileFilter);
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
