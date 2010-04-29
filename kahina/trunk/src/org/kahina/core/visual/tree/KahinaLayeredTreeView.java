package org.kahina.core.visual.tree;

import java.awt.Color;

import javax.swing.JComponent;

import org.kahina.core.KahinaRunner;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.visual.KahinaView;

public class KahinaLayeredTreeView extends KahinaView<KahinaTree>
{
	
	private static final boolean verbose = true;

	private final int firstLayer;

	private final int secondLayer;

	private KahinaTreeView overview;

	private KahinaTreeView detailView;

	private KahinaTree secondaryModel;

	public KahinaLayeredTreeView(int firstLayer, int secondLayer)
	{
		this.firstLayer = firstLayer;
		this.secondLayer = secondLayer;
		overview = new KahinaTreeView();
		overview.setLineShapePolicy(KahinaTreeView.STRAIGHT_LINES);
		overview.setNodePositionPolicy(KahinaTreeView.CENTERED_NODES);
		overview.setSecondaryLineShapePolicy(KahinaTreeView.INVISIBLE_LINES);
		overview.toggleSecondDimensionDisplay();
		overview.setVerticalDistance(3);
		overview.setHorizontalDistance(18);
		detailView = new KahinaTreeView();
		KahinaRunner.getControl().registerListener("update", overview);
		KahinaRunner.getControl().registerListener("update", detailView);
	}

	@Override
	public void doDisplay()
	{
		int rootID = model.getRootID();
		overview.display(model, firstLayer, rootID);
		detailView.display(model, secondLayer, rootID);
	}

	@Override
	public KahinaTree getModel()
	{
		return overview.getModel();
	}
    
    public KahinaTree getSecondaryModel()
    {
        return overview.secondaryTreeModel;
    }

	public void displaySecondaryTree(KahinaTree treeModel)
	{
		this.secondaryModel = treeModel;
		overview.displaySecondaryTree(treeModel);
		detailView.displaySecondaryTree(treeModel);
	}

	@Override
	public JComponent wrapInPanel()
	{
		KahinaLayeredTreeViewPanel panel = new KahinaLayeredTreeViewPanel(model, secondaryModel);
		KahinaRunner.getControl().registerListener("redraw", panel);
		panel.setView(this);
		return panel;
	}

	public KahinaTreeView getOverview()
	{
		return overview;
	}

	public KahinaTreeView getDetailView()
	{
		return detailView;
	}

	public void setStatusColorEncoding(int status, Color color)
	{
		overview.setStatusColorEncoding(status, color);
		detailView.setStatusColorEncoding(status, color);
	}
	
}
