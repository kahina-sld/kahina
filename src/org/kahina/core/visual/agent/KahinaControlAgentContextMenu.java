package org.kahina.core.visual.agent;

import java.awt.Insets;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

public class KahinaControlAgentContextMenu extends JPopupMenu
{
    private static final long serialVersionUID = 2789903959687639683L;
    
    KahinaControlAgentViewPanel v;
    
    public KahinaControlAgentContextMenu(KahinaControlAgentListener pointListener, 
                                         KahinaControlAgentProfileListener profileListener,
                                         KahinaControlAgentViewPanel v)
    {
        JMenuItem exportPatternItem = new JMenuItem("Export");
        exportPatternItem.setActionCommand("exportControlPoint");
        exportPatternItem.addActionListener(pointListener);
        add(exportPatternItem);
        
        JMenuItem removePatternItem = new JMenuItem("Remove");
        removePatternItem.setActionCommand("removeControlPoint");
        removePatternItem.addActionListener(profileListener);
        add(removePatternItem);
        
        addSeparator();
        
        JMenuItem toggleActivationItem = new JMenuItem(generateActivationItemLabel(v));
        toggleActivationItem.setActionCommand("toggleActivation");
        toggleActivationItem.addActionListener(pointListener);
        add(toggleActivationItem);
        
        JMenuItem suggestNameItem = new JMenuItem("Suggest Name");
        suggestNameItem.setActionCommand("suggestName");
        suggestNameItem.addActionListener(pointListener);
        add(suggestNameItem);
        
        JMenuItem renameItem = new JMenuItem("Rename");
        renameItem.setActionCommand("rename");
        renameItem.addActionListener(pointListener);
        add(renameItem);
        
        JMenuItem colorItem = new JMenuItem("Change Signal Color");
        colorItem.setBackground(v.view.getModel().getSignalColor());
        colorItem.setActionCommand("changeColor");
        colorItem.addActionListener(pointListener);
        add(colorItem);   
    }
    
    private static String generateActivationItemLabel(KahinaControlAgentViewPanel v)
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
    
    public static JPopupMenu getMenu(KahinaControlAgentListener pointListener, 
                                     KahinaControlAgentProfileListener profileListener,
                                     KahinaControlAgentViewPanel v)
    {
        return new KahinaControlAgentContextMenu(pointListener, profileListener, v);
    }
}
