package org.kahina.core.visual.tree;

import java.awt.Color;

import org.kahina.core.KahinaException;
import org.kahina.core.data.KahinaTypeException;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.visual.KahinaView;

public class KahinaLayeredTreeView extends KahinaView<KahinaTree>
{
	
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
        overview.setVerticalDistance(3);
        overview.setHorizontalDistance(18);
		detailView = new KahinaTreeView();
	}
	
	@Override
	public void display(KahinaTree treeModel)
	{
		super.display(treeModel);
		try
		{
			int rootID = treeModel.getRootID();
			overview.display(treeModel, firstLayer, rootID);
			detailView.display(treeModel, secondLayer, rootID);
		} catch (KahinaTypeException e)
		{
			throw new KahinaException("Unexpected type error.", e);
		}
	}
	
	@Override
	public KahinaTree getModel()
	{
		return overview.getModel();
	}
	
	public void displaySecondaryTree(KahinaTree treeModel)
	{
		this.secondaryModel = treeModel;
		overview.displaySecondaryTree(treeModel);
		detailView.displaySecondaryTree(treeModel);
	}

	@Override
	public KahinaLayeredTreeViewPanel wrapInPanel()
	{
		KahinaLayeredTreeViewPanel panel = new KahinaLayeredTreeViewPanel(model, secondaryModel);
		panel.setView(this);
		return panel;
	}
	
	@Override
	public void recalculate()
	{
		overview.recalculate();
		detailView.recalculate();
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
