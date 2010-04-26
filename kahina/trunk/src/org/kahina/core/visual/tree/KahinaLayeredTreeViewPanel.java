package org.kahina.core.visual.tree;

import javax.swing.BoxLayout;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;

import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.visual.KahinaViewPanel;

public class KahinaLayeredTreeViewPanel extends KahinaViewPanel<KahinaLayeredTreeView>
{
	private static final long serialVersionUID = -1304882362038211887L;

	private KahinaTreeViewPanel overviewPanel;

	private KahinaTreeViewPanel detailPanel;

	public KahinaLayeredTreeViewPanel(KahinaTree model, KahinaTree secondaryModel)
	{
		KahinaTreeViewMarker marker = new KahinaTreeViewMarker(model, secondaryModel);
		overviewPanel = new KahinaTreeViewPanel(marker);
		detailPanel = new KahinaTreeViewPanel(marker);
		this.setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		add(createSplitPane());
	}

	private JSplitPane createSplitPane()
	{
		JScrollPane overviewScrollPane = new JScrollPane(overviewPanel);
		JScrollPane detailScrollPane = new JScrollPane(detailPanel);		
		return new JSplitPane(JSplitPane.VERTICAL_SPLIT, overviewScrollPane, detailScrollPane);
	}

	@Override
	public void setView(KahinaLayeredTreeView view)
	{
		super.setView(view);
		overviewPanel.setView(view.getOverview());
		detailPanel.setView(view.getDetailView());
	}

	@Override
	public void updateDisplay()
	{
		overviewPanel.updateDisplay();
		detailPanel.updateDisplay();
	}

}
