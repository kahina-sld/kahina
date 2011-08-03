package org.kahina.qtype;

import org.kahina.core.control.KahinaController;
import org.kahina.lp.behavior.LogicProgrammingTreeBehavior;
import org.kahina.qtype.gui.QTypeGUI;
import org.kahina.sicstus.SICStusPrologDebuggerInstance;
import org.kahina.sicstus.SICStusPrologStep;

public class QTypeDebuggerInstance extends SICStusPrologDebuggerInstance
{

	@Override
	protected QTypeGUI createGUI(KahinaController guiController)
	{
		return new QTypeGUI(SICStusPrologStep.class, this, guiController);
	}

	@Override
	protected void createTreeBehavior()
	{
		LogicProgrammingTreeBehavior behavior = new LogicProgrammingTreeBehavior(state.getStepTree(), this, state.getSecondaryStepTree());
		behavior.setMaxNodeLabelLength(-1);
	}

}
