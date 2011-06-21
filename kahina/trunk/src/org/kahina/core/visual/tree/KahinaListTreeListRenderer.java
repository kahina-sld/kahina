package org.kahina.core.visual.tree;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.Insets;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;

public class KahinaListTreeListRenderer extends DefaultListCellRenderer
{
	KahinaListTreeViewPanel view;
	int layer;
	
	public KahinaListTreeListRenderer(KahinaListTreeViewPanel view, int layer)
	{
		this.view = view;
		this.layer = layer;
	}
	
	public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) 
	{
        JPanel cellPanel = new JPanel();
        cellPanel.setLayout(new BoxLayout(cellPanel, BoxLayout.X_AXIS));
        cellPanel.setSize(200,20);
		int nodeID = (Integer) value;
        cellPanel.add(Box.createRigidArea(new Dimension(view.getIndentationDepth(layer,nodeID) * 30,0)));
		int numAlternatives = view.getNumberOfPrimaryAlternatives(nodeID);
        if (numAlternatives > 1)
        {
            JButton leftButton = new JButton("<");
            leftButton.setMargin(new Insets(0,0,0,0));
            leftButton.setPreferredSize(new Dimension(18,15));
            if (view.view.primaryChildChoices.get(view.getChoiceParent(nodeID)) == 0)
            {
                leftButton.setEnabled(false);
            }
            cellPanel.add(leftButton);
            JButton rightButton = new JButton(">");
            rightButton.setMargin(new Insets(0,0,0,0));
            rightButton.setPreferredSize(new Dimension(18,15));
            if (view.view.primaryChildChoices.get(view.getChoiceParent(nodeID)) == numAlternatives - 1)
            {
                rightButton.setEnabled(false);
            }
            cellPanel.add(rightButton);
        }
        else
        {
            cellPanel.add(Box.createRigidArea(new Dimension(30,0)));
        }
        String entry = "";
        if (view.view.getSecondaryModel().isCollapsed(nodeID))
        {
            entry += "+ ";
        }
        else
        {
            entry += "- ";
        }
        entry += view.view.getModel().getNodeCaption(nodeID);
        JLabel entryLabel = new JLabel(entry);
        if (nodeID == view.view.getMarkedNode(layer)) 
        {
          entryLabel.setBackground(Color.yellow);
        } 
        else 
        {
          entryLabel.setBackground(Color.white);
        }
        Color fontColor = view.view.getNodeColor(nodeID);
        if (fontColor == Color.white) fontColor = Color.black;
        entryLabel.setForeground(fontColor);
        if (!view.view.getSecondaryModel().getChildren(nodeID).equals(view.view.getSecondaryModel().getChildren(nodeID, layer, false)))
        {
        	entryLabel.setFont(new Font("Arial", Font.BOLD, 12));
        }
        else
        {
        	entryLabel.setFont(new Font("Arial", Font.PLAIN, 12));
        }
        cellPanel.add(entryLabel);
        //setBorder(new EmptyBorder(6,20,6,20));    
        return cellPanel;
	}
}
