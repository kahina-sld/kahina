package org.kahina.core.gui;

import javax.swing.ImageIcon;
import javax.swing.JButton;

import org.kahina.core.io.util.IconUtil;

public class KahinaControlButton
{
    String iconPath;
    String command;
    String toolTipText;
    int mnemonic;
    
    public KahinaControlButton(String iconPath, String command, String toolTipText)
    {
        this.iconPath = iconPath;
        this.command = command;
        this.toolTipText = toolTipText;
    }
    
    public KahinaControlButton(String iconPath, String command, String toolTipText, int mnemonic)
    {
        this.iconPath = iconPath;
        this.command = command;
        this.toolTipText = toolTipText;
        this.mnemonic = mnemonic;
    }
    
    public JButton create()
    {
        JButton button = new JButton();
        if (iconPath != null)
        {
            button.setIcon(new ImageIcon(IconUtil.getIcon(iconPath)));
        }
        else
        {
            button.setText(toolTipText);
        }
        button.setActionCommand(command);
        //System.err.println("Setting mnemonic for button \"" + command + "\": " + mnemonic);
        button.setMnemonic(mnemonic);
        button.setToolTipText(toolTipText);
        return button;
    }
}
