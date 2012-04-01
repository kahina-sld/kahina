package org.kahina.qtype;

import org.kahina.core.data.breakpoint.KahinaBreakpointProfile;
import org.kahina.core.gui.KahinaViewRegistry;
import org.kahina.lp.behavior.LogicProgrammingTreeBehavior;
import org.kahina.qtype.bridge.QTypeBridge;
import org.kahina.qtype.data.bindings.QTypeGoal;
import org.kahina.qtype.gui.QTypeGUI;
import org.kahina.qtype.visual.bindings.QTypeGoalView;
import org.kahina.sicstus.SICStusPrologDebuggerInstance;

public class QTypeDebuggerInstance extends SICStusPrologDebuggerInstance
{	
	private QTypeCommander commander;
	private KahinaBreakpointProfile breakpoints;
	
	public QTypeDebuggerInstance()
	{
		super();
		commander = new QTypeCommander(this);
		commander.initializeForNewSession();
		breakpoints = new KahinaBreakpointProfile();
	}
	
	@Override
	public QTypeBridge startNewSession()
	{
		QTypeBridge bridge = (QTypeBridge) super.startNewSession(); 
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
	
	public KahinaBreakpointProfile getBreakpoints()
	{
	    return breakpoints;
	}
	
	@Override
	public String getApplicationName()
	{
		return "Kahina for QType";
	}

}
