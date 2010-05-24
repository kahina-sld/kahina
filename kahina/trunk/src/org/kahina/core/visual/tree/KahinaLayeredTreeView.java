package org.kahina.core.visual.tree;

import java.awt.Color;

import javax.swing.JComponent;

import org.kahina.core.KahinaRunner;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.core.visual.KahinaView;

public class KahinaLayeredTreeView extends KahinaView<KahinaTree>
{
	private static final boolean verbose = false;

	private final int[] layers;

	private final KahinaTreeView[] views;

	private KahinaTreeViewMarker marker;

	private KahinaTree secondaryModel;

	public KahinaLayeredTreeView(int... layers)
	{
		if (verbose)
		{
			System.out.println("Constructing " + this);
		}
		this.layers = layers;
		views = new KahinaTreeView[layers.length];
		views[0] = new KahinaTreeView();
		views[0].setLineShapePolicy(KahinaTreeView.STRAIGHT_LINES);
		views[0].setNodePositionPolicy(KahinaTreeView.CENTERED_NODES);
		views[0].setSecondaryLineShapePolicy(KahinaTreeView.INVISIBLE_LINES);
		views[0].toggleSecondDimensionDisplay();
		views[0].setVerticalDistance(3);
		views[0].setHorizontalDistance(18);
		for (int i = 1; i < views.length; i++)
		{
			views[i] = new KahinaTreeView();
			KahinaRunner.getControl().registerListener("update", views[i]);
		}
	}

	@Override
	public void doDisplay()
	{
		int rootID = model.getRootID();
		for (int i = 0; i < views.length; i++)
		{
			views[i].display(model, layers[i], rootID);
			views[i].display(model, layers[i], rootID);
		}
	}

	@Override
	public KahinaTree getModel()
	{
		return views[0].getModel();
	}

	public KahinaTree getSecondaryModel()
	{
		return views[0].secondaryTreeModel;
	}

	public void displaySecondaryTree(KahinaTree treeModel)
	{
		this.secondaryModel = treeModel;
		for (int i = 0; i < views.length; i++)
		{
			views[i].displaySecondaryTree(treeModel);
			views[i].displaySecondaryTree(treeModel);
		}
	}

	@Override
	public JComponent wrapInPanel()
	{
		marker = new KahinaTreeViewMarker(model, secondaryModel);
		KahinaLayeredTreeViewPanel panel = new KahinaLayeredTreeViewPanel(model, secondaryModel, views.length, marker);
		KahinaRunner.getControl().registerListener("redraw", panel);
		panel.setView(this);
		return panel;
	}

	public KahinaTreeView getView(int index)
	{
		return views[index];
	}

	public void setStatusColorEncoding(int status, Color color)
	{
		for (KahinaTreeView view : views)
		{
			view.setStatusColorEncoding(status, color);
		}
	}

	public void selectStep(int stepID)
	{
		if (marker != null)
		{
			marker.markNode(stepID);
		}
	}

	protected void processEvent(KahinaUpdateEvent e)
	{
		// recalculation is implicitly part of this (via marker)
		selectStep(e.getSelectedStep());
	}
}
