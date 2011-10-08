package org.kahina.core.visual.tree;

import java.awt.Color;

import javax.swing.JComponent;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.core.visual.KahinaViewConfiguration;

public class KahinaLayeredTreeView extends KahinaAbstractTreeView
{
	private static final boolean VERBOSE = false;

	private final int[] layers;

	private final KahinaTreeView[] views;

	private KahinaTreeViewMarker marker;

	private KahinaTree secondaryModel;

	private final KahinaLayeredTreeViewPanel.Orientation orientation;

	public KahinaLayeredTreeView(KahinaLayeredTreeViewPanel.Orientation orientation, KahinaController control, int... layers)
	{
		this(false, orientation, control, layers);
	}

	public KahinaLayeredTreeView(boolean displaySecondDimensionInTopLayer, KahinaLayeredTreeViewPanel.Orientation orientation, KahinaController control, int... layers)
	{
		super(control);
		if (VERBOSE)
		{
			System.out.println("Constructing " + this);
		}
		this.orientation = orientation;
		this.layers = layers;
		views = new KahinaTreeView[layers.length];
		views[0] = new KahinaTreeView(control);

		if (!displaySecondDimensionInTopLayer)
		{
			views[0].getConfig().setLineShapePolicy(KahinaTreeViewOptions.STRAIGHT_LINES);
			views[0].getConfig().setNodePositionPolicy(KahinaTreeViewOptions.CENTERED_NODES);
			views[0].getConfig().setSecondaryLineShapePolicy(KahinaTreeViewOptions.INVISIBLE_LINES);
			views[0].getConfig().toggleSecondDimensionDisplay();
			views[0].getConfig().setVerticalDistance(3);
			views[0].getConfig().setHorizontalDistance(18);
		}

		for (int i = 1; i < views.length; i++)
		{
			views[i] = new KahinaTreeView(control);
			control.registerListener("update", views[i]);
		}
	}

	@Override
	public void doDisplay()
	{
		if (VERBOSE)
		{
			System.err.println(this + ".doDisplay()");
		}
		if (marker != null)
		{
			marker.setModel(model);
		}
		int rootID = model.getRootID();
		for (int i = 0; i < views.length; i++)
		{
			if (VERBOSE)
			{
				System.err.println("Displaying layer #" + i);
			}
			views[i].display(model, layers[i], rootID);
		}
		if (VERBOSE)
		{
			System.err.println("//" + this + ".doDisplay()");
		}
	}

	@Override
	public void displaySecondaryTree(KahinaTree treeModel)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".displaySecondaryTree(" + treeModel + ")");
		}
		if (marker != null)
		{
			marker.setSecondaryModel(treeModel);
		}
		if (VERBOSE)
		{
			System.err.println("Secondary model set in marker.");
		}
		this.secondaryModel = treeModel;
		for (int i = 0; i < views.length; i++)
		{
			views[i].displaySecondaryTree(treeModel);
		}
	}

	@Override
	public KahinaTree getModel()
	{
		return views[0].getModel();
	}

	public KahinaTree getSecondaryModel()
	{
		return views[0].getSecondaryModel();
	}

	@Override
	public JComponent makePanel(KahinaGUI gui)
	{
		marker = new KahinaTreeViewMarker(model, secondaryModel);
		KahinaLayeredTreeViewPanel panel = new KahinaLayeredTreeViewPanel(views.length, marker, control, orientation);
		control.registerListener("redraw", panel);
		panel.setView(this);
		return panel;
	}

	public KahinaTreeView getView(int index)
	{
		return views[index];
	}

	@Override
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

	@Override
	protected void processEvent(KahinaUpdateEvent e)
	{
		// recalculation is implicitly part of this (via marker)
		selectStep(e.getSelectedStep());
	}

	// TODO: define a configuration that does not only configure the top layer
	public KahinaTreeViewConfiguration getConfig()
	{
		return views[0].getConfig();
	}

	// TODO: define a configuration that does not only configure the top layer
	public void setConfig(KahinaViewConfiguration config)
	{
		System.err.print(((KahinaTreeViewConfiguration) config).bgColor + " ");
		System.err.println(((KahinaTreeViewConfiguration) config).getVerticalDistance());
		views[0].setConfig((KahinaTreeViewConfiguration) config);
	}
}
