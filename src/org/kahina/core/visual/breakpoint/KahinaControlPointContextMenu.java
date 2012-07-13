package org.kahina.core.visual.breakpoint;

import java.awt.Insets;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

public class KahinaControlPointContextMenu extends JPopupMenu
{
    private static final long serialVersionUID = 2789903959687639683L;
    
    KahinaControlPointViewPanel v;
    
    public KahinaControlPointContextMenu(ActionListener l, KahinaControlPointViewPanel v)
    {
        JMenuItem exportPatternItem = new JMenuItem("Export");
        exportPatternItem.setActionCommand("exportControlPoint");
        exportPatternItem.addActionListener(l);
        add(exportPatternItem);
        
        JMenuItem removePatternItem = new JMenuItem("Remove");
        removePatternItem.setActionCommand("removeControlPoint");
        removePatternItem.addActionListener(l);
        add(removePatternItem);
        
        addSeparator();
        
        JMenuItem toggleActivationItem = new JMenuItem(generateActivationItemLabel(v));
        toggleActivationItem.setActionCommand("toggleActivation");
        toggleActivationItem.addActionListener(l);
        add(toggleActivationItem);
        
        JMenuItem suggestNameItem = new JMenuItem("Suggest Name");
        suggestNameItem.setActionCommand("suggestName");
        suggestNameItem.addActionListener(l);
        add(suggestNameItem);
        
        JMenuItem renameItem = new JMenuItem("Rename");
        renameItem.setActionCommand("rename");
        renameItem.addActionListener(l);
        add(renameItem);
        
        JMenuItem colorItem = new JMenuItem("Change Signal Color");
        colorItem.setBackground(v.view.getModel().getSignalColor());
        colorItem.setActionCommand("changeColor");
        colorItem.addActionListener(l);
        add(colorItem);   
    }
    
    private static String generateActivationItemLabel(KahinaControlPointViewPanel v)
    {
        if (v.view.getModel().isActive())
        {
            return("Deactivate");
        }
        else
        {
            return("Activate");
        }
    }
    
    public static JPopupMenu getMenu(ActionListener l, KahinaControlPointViewPanel v)
    {
        return new KahinaControlPointContextMenu(l, v);
    }
}
