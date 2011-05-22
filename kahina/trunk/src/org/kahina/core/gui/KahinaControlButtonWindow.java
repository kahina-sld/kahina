package org.kahina.core.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.LinkedList;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JButton;

import org.kahina.core.KahinaRunner;
import org.kahina.core.event.KahinaControlEvent;

public class KahinaControlButtonWindow extends KahinaWindow implements ActionListener
{
	List<KahinaControlButton> buttons;
	
	public KahinaControlButtonWindow(KahinaWindowManager wm) 
	{
		super(wm);
		buttons = new LinkedList<KahinaControlButton>();
	}
	
	public KahinaControlButtonWindow(KahinaWindowManager wm, int winID) 
	{
		super(wm,winID);
		buttons = new LinkedList<KahinaControlButton>();
	}
	
    //used to add simple button definitions (not more than an icon path, a command, a tool tip and a mnemonic)
    public void addControlButton(String iconFilePath, String command, String toolTipText, int mnemonic)
    {
        buttons.add(new KahinaControlButton(iconFilePath, command, toolTipText, mnemonic));       
    }
    
    //needs to be built before the window can be displayed
    public void build()
    {
        this.removeAll();
        this.setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
        for (KahinaControlButton controlButton : buttons)
        {
            JButton button = controlButton.create();
            button.addActionListener(this);
            add(button);
        }
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String command = e.getActionCommand();
        KahinaRunner.processEvent(new KahinaControlEvent(command));
    }
}