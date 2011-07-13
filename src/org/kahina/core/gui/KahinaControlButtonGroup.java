package org.kahina.core.gui;

import java.util.ArrayList;
import java.util.List;

public class KahinaControlButtonGroup
{
    //definitions of simple buttons
    List<KahinaControlButton> controlButtons;
    
    String groupCaption;
    
    public KahinaControlButtonGroup(String groupCaption)
    {
        controlButtons = new ArrayList<KahinaControlButton>();
        this.groupCaption = groupCaption;
    }
    
    //used to add simple button definitions (not more than an icon path, a command, a tool tip and a hotkey)
    public void addControlButton(String iconFilePath, String command, String toolTipText, int mnemonic)
    {
        controlButtons.add(new KahinaControlButton(iconFilePath, command, toolTipText, mnemonic));       
    }
}
