package org.kahina.core.visual.tree;

import java.awt.Color;
import java.awt.Component;

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
        String entry = view.getIndentingWhitespace(layer,nodeID) + view.view.getModel().getNodeCaption(nodeID);
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
        //setBorder(new EmptyBorder(6,20,6,20));    
        return this;
	}
}
