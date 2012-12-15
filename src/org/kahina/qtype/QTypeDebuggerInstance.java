package org.kahina.qtype;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.LinkedList;

import org.kahina.core.gui.KahinaPerspective;
import org.kahina.core.gui.KahinaViewRegistry;
import org.kahina.core.io.util.XMLUtil;
import org.kahina.lp.LogicProgrammingInstance;
import org.kahina.lp.behavior.LogicProgrammingTreeBehavior;
import org.kahina.lp.profiler.LogicProgrammingProfiler;
import org.kahina.prolog.profiler.PrologProfiler;
import org.kahina.qtype.bridge.QTypeBridge;
import org.kahina.qtype.data.bindings.QTypeGoal;
import org.kahina.qtype.data.project.QTypeProject;
import org.kahina.qtype.gui.QTypeGUI;
import org.kahina.qtype.visual.bindings.QTypeGoalView;
import org.kahina.sicstus.bridge.SICStusPrologBridge;
import org.w3c.dom.Document;

public class QTypeDebuggerInstance extends LogicProgrammingInstance<QTypeState, QTypeGUI, SICStusPrologBridge, QTypeProject>
{		
	private QTypeCommander commander;
	
	PrologProfiler profiler;
	
	public QTypeDebuggerInstance()
	{
		super();
		commander = new QTypeCommander(this);
		commander.initializeForNewSession();
	}
	
	@Override
	public SICStusPrologBridge startNewSession()
	{
	    try
        {
            bridge = (QTypeBridge) super.startNewSession();
            profiler = new PrologProfiler(this, state.getFullProfile());
            commander.initializeForNewSession();
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
		return "Kahina for QType";
	}
    
     @Override
    protected QTypeProject createNewProject()
    {
        return new QTypeProject("no name", state.getStepTree(), this);
    }
        
    public QTypeProject loadProject(InputStream stream)
    {
        //System.err.println("QTypeProject.loadProject");
        Document dom;
        QTypeProject project = createNewProject();
        dom = XMLUtil.parseXMLStream(stream, false);
        project = QTypeProject.importXML(dom.getDocumentElement(), project, this, state.getStepTree());
        return project;
    }
    
    @Override
    protected void prepareProjectLists()
    {
        recentProjects = new LinkedList<QTypeProject>();
        defaultProjects = new LinkedList<QTypeProject>();
        addDefaultProject("data/project/qtype-tutorial1-project.xml");
        addDefaultProject("data/project/qtype-tutorial2-project.xml");
        addDefaultProject("data/project/qtype-demo-project.xml");
    } 
    
    private void addDefaultProject(String resourcePath)
    {
        URL projectLocation = this.getClass().getResource(resourcePath);
        try
        {
            InputStream projectInputStream = projectLocation.openStream();
            //System.err.println("Loading default project: " + resourcePath);
            defaultProjects.add(loadProject(projectInputStream));
            //System.err.println("Loaded default project: " + defaultProjects.get(defaultProjects.size() - 1));
        }
        catch (IOException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
    
    @Override
    protected void preparePerspectiveLists()
    {
        recentPerspectives = new LinkedList<KahinaPerspective>();
        defaultPerspectives = new LinkedList<KahinaPerspective>(); 
        addDefaultPerspective("gui/kahinaqtype-demo.xml");
        addDefaultPerspective("gui/kahinaqtype-integrated.xml");
    } 
    
    private void addDefaultPerspective(String resourcePath)
    {
        URL perspectiveLocation = this.getClass().getResource(resourcePath);
        try
        {
            InputStream perspectiveInputStream = perspectiveLocation.openStream();
            defaultPerspectives.add(loadPerspective(perspectiveInputStream));
        }
        catch (IOException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
    
    public QTypeGUI getGUI()
    {
        return (QTypeGUI) gui;
    }

    @Override
    public LogicProgrammingProfiler getProfiler()
    {
        return profiler;
    }

    @Override
    protected QTypeState createState()
    {
        return new QTypeState(this);
    }
}
