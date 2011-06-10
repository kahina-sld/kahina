package org.kahina.core.visual.tree;

import java.awt.Color;

import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.ListModel;

import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.KahinaViewPanel;

public class KahinaListTreeViewPanel extends KahinaViewPanel<KahinaListTreeView>
{
	private static final boolean VERBOSE = false;
	
	private JPanel[] panels;
	private JList[] lists;
	private DefaultListModel[] listModels;
	
	public KahinaListTreeViewPanel(int layers, KahinaController control)
	{
		if (VERBOSE)
		{
			System.err.println("new KahinaListTreeViewPanel(" + layers + ")");
		}
		panels = new JPanel[layers];
		lists = new JList[layers];
		listModels = new DefaultListModel[layers];
		for (int i = 0; i < panels.length; i++)
		{
			panels[i] = new JPanel();
			lists[i] = new JList();
			listModels[i] = new DefaultListModel();
			lists[i].setModel(listModels[i]);
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
		JComponent left = createPane(panels[index]);
		JComponent right;
		index++;
		if (index + 1 == panels.length)
		{
			right = createPane(panels[index]);
		} 
		else
		{
			right = createSplitPane(index);
		}
		return new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, left, right);
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
		for (int i = 0; i < panels.length; i++)
		{
			listModels[i].clear();
			int rootID = view.secondaryTreeModel.getRootID(i);
			fillListModel(i,rootID,0);
		}
		for (JPanel panel : panels)
		{
			panel.repaint();
			panel.revalidate();
		}
	}
	
	private void fillListModel(int layer, int nodeID, int recursionDepth)
	{
		String nodeCaption = view.getModel().getNodeCaption(nodeID);
		listModels[layer].addElement(nodeCaption);
		for (int childID : view.getVisibleVirtualChildren(view.secondaryTreeModel, nodeID, layer))
		{
			fillListModel(layer, childID, recursionDepth + 1);
		}
	}
}
