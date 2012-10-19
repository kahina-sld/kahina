package org.kahina.logic.sat.visual.cnf.list;

import java.awt.Component;

import javax.swing.DefaultListCellRenderer;
import javax.swing.JList;

public class KahinaSatInstanceListCellRenderer extends DefaultListCellRenderer
{
    KahinaSatInstanceListViewPanel panel;
    
    public KahinaSatInstanceListCellRenderer(KahinaSatInstanceListViewPanel panel) 
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
