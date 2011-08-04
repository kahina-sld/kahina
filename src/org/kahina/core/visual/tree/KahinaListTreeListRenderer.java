package org.kahina.core.visual.tree;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Insets;
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;

public class KahinaListTreeListRenderer extends DefaultListCellRenderer
{
	/**
	 * 
	 */
	private static final long serialVersionUID = 331637404881122724L;

	private static final boolean VERBOSE = false;

	KahinaListTreeViewPanel view;
	int layer;

	public KahinaListTreeListRenderer(KahinaListTreeViewPanel view, int layer)
	{
		this.view = view;
		this.layer = layer;
	}

	public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".getListCellRendererComponent([list], [value], " + index + ", " + isSelected + ", " + cellHasFocus + ")");
		}
		JPanel cellPanel = new JPanel();
		cellPanel.setLayout(new BoxLayout(cellPanel, BoxLayout.X_AXIS));
		cellPanel.setSize(200, 20);
		int nodeID = (Integer) value;
		List<Integer> primaryAlternatives = view.view.getPrimaryAlternatives(nodeID, layer);
		if (VERBOSE)
		{
			System.err.println("Primary alternatives of node " + nodeID + " (" + view.view.getTreeModel().getNodeCaption(nodeID) + "): " + primaryAlternatives);
		}
		int numAlternatives = primaryAlternatives.size();
		if (numAlternatives > 1 && index != 0)
		{
			if (VERBOSE)
			{
				System.err.println("numAlternatives: " + numAlternatives);
			}
			JButton leftButton = new JButton("<");
			leftButton.setMargin(new Insets(0, 0, 0, 0));
			leftButton.setPreferredSize(new Dimension(18, 15));
			if (view.view.isChosen(primaryAlternatives.get(0)))
			{
				leftButton.setEnabled(false);
			}
			cellPanel.add(leftButton);
			JButton rightButton = new JButton(">");
			rightButton.setMargin(new Insets(0, 0, 0, 0));
			rightButton.setPreferredSize(new Dimension(18, 15));
			if (view.view.isChosen(primaryAlternatives.get(numAlternatives - 1)))
			{
				rightButton.setEnabled(false);
			}
			cellPanel.add(rightButton);
		} else
		{
			cellPanel.add(Box.createRigidArea(new Dimension(30, 0)));
		}
		cellPanel.add(Box.createRigidArea(new Dimension(view.getIndentationDepth(layer, nodeID) * 15, 0)));
		String entry = "";
		if (view.view.getSecondaryModel().isCollapsed(nodeID))
		{
			entry += "+ ";
		} else
		{
			entry += "- ";
		}
		entry += view.view.getModel().getNodeCaption(nodeID);
		JLabel entryLabel = new JLabel(entry);
		entryLabel.setOpaque(true);
		if (nodeID == view.view.getMarkedNode(layer))
		{
			cellPanel.setBackground(Color.white);
			entryLabel.setBackground(Color.yellow);
		} else
		{
			cellPanel.setBackground(Color.white);
			entryLabel.setBackground(Color.white);
		}
		Color fontColor = view.view.getNodeColor(nodeID);
		if (fontColor == Color.white)
			fontColor = Color.black;
		entryLabel.setForeground(fontColor);
		if (!view.view.getSecondaryModel().getChildren(nodeID).equals(view.view.getSecondaryModel().getChildren(nodeID, layer, false)))
		{
			entryLabel.setFont(new Font("Arial", Font.BOLD, 12));
		} else
		{
			entryLabel.setFont(new Font("Arial", Font.PLAIN, 12));
		}
		cellPanel.add(entryLabel);
		// setBorder(new EmptyBorder(6,20,6,20));
		return cellPanel;
	}
}
