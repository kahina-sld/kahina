package org.kahina.core.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;

import org.kahina.core.KahinaRunner;
import org.kahina.core.event.KahinaControlEvent;

public class KahinaControlPanel extends JPanel implements ActionListener
{
	private static final long serialVersionUID = 6440832833800241356L;
	
	//definitions of simple buttons
    List<KahinaControlButton> controlButtons;
    
    public KahinaControlPanel()
    {
        super();
        controlButtons = new ArrayList<KahinaControlButton>();
    }
    
    //used to add simple button definitions (not more than an icon path, a command and a tool tip)
    public void addControlButton(String iconFilePath, String command, String toolTipText)
    {
        controlButtons.add(new KahinaControlButton(iconFilePath, command, toolTipText));       
    }
    
    //used to fill the panel with the content defined by the controlButtons
    public void build()
    {
        this.removeAll();
        this.setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
        for (KahinaControlButton controlButton : controlButtons)
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
