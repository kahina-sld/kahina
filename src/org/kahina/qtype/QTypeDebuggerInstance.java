package org.kahina.qtype;

import org.kahina.core.control.KahinaController;
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
	
	public QTypeDebuggerInstance()
	{
		super();
		commander = new QTypeCommander(control);
		commander.initializeForNewSession();
	}
	
	@Override
	public QTypeBridge startNewSession()
	{
		QTypeBridge bridge = (QTypeBridge) super.startNewSession(); 
		return bridge;
	}

	@Override
	protected QTypeGUI createGUI(KahinaController guiController)
	{
		return new QTypeGUI(QTypeStep.class, this, guiController);
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

}
