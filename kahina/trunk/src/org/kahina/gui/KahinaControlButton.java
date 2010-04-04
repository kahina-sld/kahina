package org.kahina.gui;

import javax.swing.ImageIcon;
import javax.swing.JButton;

import org.kahina.gui.icons.IconUtil;

public class KahinaControlButton
{
    String iconPath;
    String command;
    String toolTipText;
    
    public KahinaControlButton(String iconPath, String command, String toolTipText)
    {
        this.iconPath = iconPath;
        this.command = command;
        this.toolTipText = toolTipText;
    }
    
    public JButton create()
    {
        JButton button = new JButton();
        button.setIcon(new ImageIcon(IconUtil.getIcon(iconPath)));
        button.setActionCommand(command);
        button.setToolTipText(toolTipText);
        return button;
    }
}
