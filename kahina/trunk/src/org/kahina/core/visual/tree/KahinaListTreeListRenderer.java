package org.kahina.core.visual.tree;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;

import javax.swing.DefaultListCellRenderer;
import javax.swing.JList;

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
		int nodeID = (Integer) value;
        String entry = view.getIndentingWhitespace(layer,nodeID);
        if (view.view.getSecondaryModel().isCollapsed(nodeID))
        {
            entry += "+ ";
        }
        else
        {
            entry += "- ";
        }
        entry += view.view.getModel().getNodeCaption(nodeID);
        setText(entry);
        if (nodeID == view.view.getMarkedNode(layer)) 
        {
          setBackground(Color.yellow);
        } 
        else 
        {
          setBackground(Color.white);
        }
        Color fontColor = view.view.getNodeColor(nodeID);
        if (fontColor == Color.white) fontColor = Color.black;
        setForeground(fontColor);
        if (!view.view.getSecondaryModel().getChildren(nodeID).equals(view.view.getSecondaryModel().getChildren(nodeID, layer, false)))
        {
        	this.setFont(new Font("Arial", Font.BOLD, 12));
        }
        else
        {
        	this.setFont(new Font("Arial", Font.PLAIN, 12));
        }
        //setBorder(new EmptyBorder(6,20,6,20));    
        return this;
	}
}
