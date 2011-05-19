package org.kahina.core.gui;

import java.util.List;

public class KahinaControlButtonWindow extends KahinaWindow
{
	List<KahinaControlButton> buttons;
	
	public KahinaControlButtonWindow(KahinaWindowManager wm) 
	{
		super(wm);
	}
	
	public KahinaControlButtonWindow(KahinaWindowManager wm, int winID) 
	{
		super(wm,winID);
	}
	
    //used to add simple button definitions (not more than an icon path, a command, a tool tip and a mnemonic)
    public void addControlButton(String iconFilePath, String command, String toolTipText, int mnemonic)
    {
        buttons.add(new KahinaControlButton(iconFilePath, command, toolTipText, mnemonic));       
    }
}
