package org.kahina.qtype.gui;

import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.tree.KahinaListTreeView;
import org.kahina.qtype.data.tree.QTypeLayerDecider;
import org.kahina.sicstus.SICStusPrologDebuggerInstance;
import org.kahina.sicstus.SICStusPrologStep;
import org.kahina.sicstus.gui.SICStusPrologGUI;

public class QTypeGUI extends SICStusPrologGUI
{

	public QTypeGUI(Class<? extends SICStusPrologStep> stepType, SICStusPrologDebuggerInstance instance, KahinaController control)
	{
		super(stepType, instance, control);
	}

	@Override
	protected KahinaListTreeView generateTreeView(KahinaController control)
	{
		return new KahinaListTreeView(control, 0, 1);
	}

	@Override
	public void displayMainViews()
	{
		super.displayMainViews();
		mainTreeView.getModel().setLayerDecider(new QTypeLayerDecider());
		mainTreeView.getSecondaryModel().setLayerDecider(new QTypeLayerDecider());
	}

}
