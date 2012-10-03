package org.kahina.core.gui;

import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaActivationEvent;
import org.kahina.core.control.KahinaActivationStatus;
import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.gui.windows.KahinaWindow;
import org.kahina.core.gui.windows.KahinaWindowType;

public class KahinaControlButtonWindow extends KahinaWindow implements ActionListener, KahinaListener
{

	private static final long serialVersionUID = -809546592680882505L;
	List<KahinaControlButton> buttons;
	Map<String,JButton> buttonByID;
	
	public KahinaControlButtonWindow(KahinaWindowManager wm, KahinaInstance<?, ?, ?, ?> kahina) 
	{
		super(wm, kahina);
		buttons = new LinkedList<KahinaControlButton>();
		buttonByID = new HashMap<String,JButton>();
		kahina.registerInstanceListener(KahinaEventTypes.ACTIVATION, this);
	}
	
	public KahinaControlButtonWindow(KahinaWindowManager wm, KahinaInstance<?, ?, ?, ?> kahina, int winID) 
	{
		super(wm, kahina, winID);
		buttons = new LinkedList<KahinaControlButton>();
	    buttonByID = new HashMap<String,JButton>();
	    kahina.registerInstanceListener(KahinaEventTypes.ACTIVATION, this);
	}
	
    //used to add simple button definitions (not more than an icon path, a command, a tool tip and a mnemonic)
    public void addControlButton(KahinaControlButton button)
    {
        buttons.add(button);       
    }
    
    //needs to be built before the window can be displayed
    public void build()
    {
        mainPanel.removeAll();
        buttonByID.clear();
        mainPanel.setLayout(new GridLayout());
        //mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.X_AXIS));
        for (KahinaControlButton controlButton : buttons)
        {
            JButton button = controlButton.create();
            button.addActionListener(this);
            buttonByID.put(controlButton.command, button);
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

    @Override
    public void processEvent(KahinaEvent event)
    {
        if (event.getType().equals(KahinaEventTypes.ACTIVATION))
        {
            processEvent((KahinaActivationEvent) event);
        }     
    }
    
    public void processEvent(KahinaActivationEvent event)
    {
        JButton button = buttonByID.get(event.getElementID());
        if (button != null)
        {
            Dimension size = button.getSize();
            switch (event.getStatus())
            {
                case ACTIVE:
                {
                    button.setEnabled(true);
                    button.setBorder(BorderFactory.createRaisedBevelBorder());
                    button.setSize(size);
                    break;
                }
                case INACTIVE:
                {
                    button.setEnabled(false);
                    break;
                }
                case PRESSED:
                {
                    button.setEnabled(true);

                    button.setBorder(BorderFactory.createLoweredBevelBorder());
                    button.setSize(size);
                    break;
                }
            }
        }
    }
}
