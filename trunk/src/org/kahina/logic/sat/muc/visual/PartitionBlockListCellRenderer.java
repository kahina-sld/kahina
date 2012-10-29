package org.kahina.logic.sat.muc.visual;

import java.awt.Component;

import javax.swing.DefaultListCellRenderer;
import javax.swing.JList;

public class PartitionBlockListCellRenderer extends DefaultListCellRenderer
{
    PartitionBlockViewPanel panel;
    
    public PartitionBlockListCellRenderer(PartitionBlockViewPanel panel) 
    {
        this.panel = panel;
    }
    
    public Component getListCellRendererComponent(JList paramlist, Object value, int index, boolean isSelected, boolean cellHasFocus) 
    {
        Component c = super.getListCellRendererComponent(paramlist, value, index, isSelected, cellHasFocus);
        c.setForeground(panel.view.getLineColor(index));
        return c;
    }
}
