package org.kahina.swi.gui;

import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.tree.KahinaLayeredTreeView;
import org.kahina.core.visual.tree.KahinaListTreeView;
import org.kahina.lp.gui.LogicProgrammingGUI;
import org.kahina.swi.SWIPrologDebuggerInstance;
import org.kahina.swi.SWIPrologStep;
import org.kahina.swi.data.tree.SWIPrologLayerDecider;

public class SWIPrologGUI extends LogicProgrammingGUI
{
	
	private static final boolean VERBOSE = false;

	public SWIPrologGUI(Class<? extends SWIPrologStep> stepType, SWIPrologDebuggerInstance instance, KahinaController control)
	{
		super(stepType, instance, control);
	}
	
	@Override
	protected KahinaListTreeView generateTreeView(KahinaController control)
	{
		return new KahinaListTreeView(control, 0, 1, 2);
	}

	/*@Override
	protected KahinaLayeredTreeView generateTreeView(KahinaController control)
	{
		return new KahinaLayeredTreeView(control, 0, 1, 2);
	}*/

	@Override
	public void displayMainViews()
	{
		if (VERBOSE)
		{
			System.err.println(this + ".displayMainViews()");
		}
		super.displayMainViews();
		if (VERBOSE)
		{
			System.err.println("Main views displayed as far as superclass of SWIPrologGUI is concerned.");
		}
		//only set deciders here because the trees are generated generically by the KahinaState
		mainTreeView.getModel().setLayerDecider(new SWIPrologLayerDecider(2));
		mainTreeView.getSecondaryModel().setLayerDecider(new SWIPrologLayerDecider(2));
		if (VERBOSE)
		{
			System.err.println("//" + this + ".displayMainViews()");
		}
	}
}
