package org.kahina.core.visual.tree;

import java.awt.Color;

import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.KahinaViewPanel;

public class KahinaListTreeViewPanel extends KahinaViewPanel<KahinaListTreeView>
{
	private static final boolean VERBOSE = false;
	
	private JPanel[] panels;
	private JList[] lists;
	
	public KahinaListTreeViewPanel(int layers, KahinaController control)
	{
		if (VERBOSE)
		{
			System.err.println("new KahinaListTreeViewPanel(" + layers + ")");
		}
		panels = new JPanel[layers];
		for (int i = 0; i < panels.length; i++)
		{
			panels[i] = new JPanel();
			lists[i] = new JList();
			panels[i].add(lists[i]);
		}
		this.setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		if (layers > 1)
		{
			add(createSplitPane(0));
		} 
		else
		{
			add(createPane(panels[0]));
		}
	}

	private JComponent createSplitPane(int index)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".createSplitPane(" + index + ")");
		}
		JComponent top = createPane(panels[index]);
		JComponent bottom;
		index++;
		if (index + 1 == panels.length)
		{
			bottom = createPane(panels[index]);
		} else
		{
			bottom = createSplitPane(index);
		}

		return new JSplitPane(JSplitPane.VERTICAL_SPLIT, top, bottom);
	}

	private JComponent createPane(JComponent panel)
	{
		JScrollPane result = new JScrollPane(panel);
		result.getVerticalScrollBar().setUnitIncrement(16);
		result.getViewport().setBackground(Color.WHITE);
		return result;
	}

	@Override
	public void updateDisplay()
	{
		for (JPanel panel : panels)
		{
			panel.repaint();
			panel.revalidate();
		}
	}
}
