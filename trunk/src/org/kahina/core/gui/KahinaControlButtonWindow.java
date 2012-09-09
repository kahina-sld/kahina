package org.kahina.core.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.LinkedList;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JButton;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.gui.windows.KahinaWindow;
import org.kahina.core.gui.windows.KahinaWindowType;

public class KahinaControlButtonWindow extends KahinaWindow implements ActionListener
{

	private static final long serialVersionUID = -809546592680882505L;
	List<KahinaControlButton> buttons;
	
	public KahinaControlButtonWindow(KahinaWindowManager wm, KahinaInstance<?, ?, ?, ?> kahina) 
	{
		super(wm, kahina);
		buttons = new LinkedList<KahinaControlButton>();
	}
	
	public KahinaControlButtonWindow(KahinaWindowManager wm, KahinaInstance<?, ?, ?, ?> kahina, int winID) 
	{
		super(wm, kahina, winID);
		buttons = new LinkedList<KahinaControlButton>();
	}
	
    //used to add simple button definitions (not more than an icon path, a command, a tool tip and a mnemonic)
    public void addControlButton(KahinaControlButton button)
    {
        buttons.add(button);       
    }
    
    //needs to be built before the window can be displayed
    public void build()
    {
        //this.removeAll();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.X_AXIS));
        for (KahinaControlButton controlButton : buttons)
        {
            JButton button = controlButton.create();
            button.addActionListener(this);
            mainPanel.add(button);
        }
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String command = e.getActionCommand();
        kahina.dispatchEvent(new KahinaControlEvent(command));
    }
    
    public int getWindowType()
    {
    	return KahinaWindowType.CONTROL_WINDOW;
    }
}
