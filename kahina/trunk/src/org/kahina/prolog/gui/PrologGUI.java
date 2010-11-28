package org.kahina.prolog.gui;

import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.tree.KahinaLayeredTreeView;
import org.kahina.lp.gui.LogicProgrammingGUI;
import org.kahina.prolog.PrologDebuggerInstance;
import org.kahina.prolog.PrologStep;
import org.kahina.prolog.data.tree.PrologLayerDecider;

public class PrologGUI extends LogicProgrammingGUI
{

	public PrologGUI(Class<? extends PrologStep> stepType, PrologDebuggerInstance instance, KahinaController control)
	{
		super(stepType, instance, control);
	}
	
	@Override
	protected KahinaLayeredTreeView generateTreeView(KahinaController control)
	{
		return new KahinaLayeredTreeView(control, 0, 1, 2);
	}

	@Override
	public void displayMainViews()
	{
		super.displayMainViews();
		//only set deciders here because the trees are generated generically by the KahinaState
		mainTreeView.getModel().setLayerDecider(new PrologLayerDecider(2));
		mainTreeView.getSecondaryModel().setLayerDecider(new PrologLayerDecider(2));
	}
}
