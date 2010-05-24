package org.kahina.core.visual.tree;

import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;

import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.visual.KahinaViewPanel;

public class KahinaLayeredTreeViewPanel extends KahinaViewPanel<KahinaLayeredTreeView>
{
	private static final long serialVersionUID = -1304882362038211887L;

	private KahinaTreeViewPanel[] panels;

	public KahinaLayeredTreeViewPanel(KahinaTree model, KahinaTree secondaryModel, int layers, KahinaTreeViewMarker marker)
	{
		panels = new KahinaTreeViewPanel[layers];
		for (int i = 0; i < panels.length; i++)
		{
			panels[i] = new KahinaTreeViewPanel(marker);
		}
		this.setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		add(createSplitPane(0));
	}

	private JComponent createSplitPane(int index)
	{
		JScrollPane top = new JScrollPane(panels[index]);
		JScrollPane bottom;
		index++;
		if (index + 1 == panels.length)
		{
			bottom = new JScrollPane(panels[index]);
		} else
		{
			bottom = new JScrollPane(createSplitPane(index));
		}	
		return new JSplitPane(JSplitPane.VERTICAL_SPLIT, top, bottom);
	}

	@Override
	public void setView(KahinaLayeredTreeView view)
	{
		super.setView(view);
		for (int i = 0; i < panels.length; i++)
		{
			panels[i].setView(view.getView(i));
		}
	}

	@Override
	public void updateDisplay()
	{
		for (KahinaTreeViewPanel panel : panels)
		{
			panel.updateDisplay();
		}
	}

}
