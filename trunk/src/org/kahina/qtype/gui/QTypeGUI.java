package org.kahina.qtype.gui;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.tree.LayerDecider;
import org.kahina.core.visual.tree.KahinaLayeredTreeView;
import org.kahina.core.visual.tree.KahinaLayeredTreeViewPanel;
import org.kahina.qtype.data.tree.QTypeLayerDecider;
import org.kahina.sicstus.SICStusPrologDebuggerInstance;
import org.kahina.sicstus.SICStusPrologStep;
import org.kahina.sicstus.gui.SICStusPrologGUI;

public class QTypeGUI extends SICStusPrologGUI {
	
	private static final LayerDecider DECIDER = new QTypeLayerDecider();

	public QTypeGUI(Class<? extends SICStusPrologStep> stepType,
			SICStusPrologDebuggerInstance instance, KahinaController control) {
		super(stepType, instance, control);
	}
	
	@Override
	protected KahinaLayeredTreeView generateTreeView(KahinaController control)
	{
		return new KahinaLayeredTreeView(true, KahinaLayeredTreeViewPanel.Orientation.HORIZONTAL, control, 0, 1);
	}
	
	@Override
	public void displayMainViews()
	{
		super.displayMainViews();
		mainTreeView.getModel().setLayerDecider(DECIDER);
		mainTreeView.getSecondaryModel().setLayerDecider(DECIDER);
	}

}
