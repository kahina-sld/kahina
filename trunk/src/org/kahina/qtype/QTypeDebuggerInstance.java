package org.kahina.qtype;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

import org.kahina.core.KahinaState;
import org.kahina.core.data.breakpoint.KahinaBreakpointProfile;
import org.kahina.core.data.project.KahinaProject;
import org.kahina.core.data.project.KahinaProjectStatus;
import org.kahina.core.gui.KahinaViewRegistry;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.io.util.XMLUtil;
import org.kahina.lp.behavior.LogicProgrammingTreeBehavior;
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
        return new QTypeProject(state.getStepTree(), control);
    }
        
    public void loadProject(File projectFile)
    {
        Document dom;
        try
        {
            dom = XMLUtil.parseXMLStream(new FileInputStream(projectFile), false);
            project = new QTypeProject(state.getStepTree(), control);
            project = QTypeProject.importXML(dom.getDocumentElement(), project, control);
            gui.setPerspective(project.getPerspective());
            gui.displayMainViews();
            setProjectStatus(KahinaProjectStatus.PROGRAM_UNCOMPILED);
            dispatchGUIEvent(new KahinaRedrawEvent());
        }
        catch (FileNotFoundException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
}
