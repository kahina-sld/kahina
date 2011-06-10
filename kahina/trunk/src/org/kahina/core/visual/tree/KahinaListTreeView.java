package org.kahina.core.visual.tree;

import java.awt.Color;

import javax.swing.JComponent;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.KahinaViewConfiguration;

public class KahinaListTreeView extends KahinaView<KahinaTree>
{
	private static final boolean VERBOSE = false;
	private final int[] layers;
	private KahinaTree secondaryModel;
	int selectedStep;
	
	public KahinaListTreeView(KahinaController control, int... layers)
	{
		super(control);
		this.layers = layers;
	}

	public void displaySecondaryTree(KahinaTree treeModel)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".displaySecondaryTree(" + treeModel + ")");
		}
		this.secondaryModel = treeModel;
	}

	public KahinaTree getSecondaryModel()
	{
		return secondaryModel;
	}

	@Override
	public JComponent wrapInPanel(KahinaController control)
	{
		KahinaListTreeViewPanel panel = new KahinaListTreeViewPanel(layers.length, control);
		control.registerListener("redraw", panel);
		panel.setView(this);
		return panel;
	}

	public void selectStep(int stepID)
	{
		selectedStep = stepID;
	}

	@Override
	protected void processEvent(KahinaUpdateEvent e)
	{
		// recalculation is implicitly part of this (via marker)
		selectStep(e.getSelectedStep());
	}
}
