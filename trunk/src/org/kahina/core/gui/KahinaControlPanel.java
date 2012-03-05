package org.kahina.core.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;

import org.kahina.core.KahinaRunner;
import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaController;

public class KahinaControlPanel extends JPanel implements ActionListener
{
	private static final long serialVersionUID = 6440832833800241356L;
	
	KahinaController control;
	
	//definitions of control button groups
    HashMap<String, KahinaControlButtonGroup> controlButtonGroups;
    
    List<String> buttonGroupOrdering;
    
    public KahinaControlPanel(KahinaController control)
    {
        super();
        this.control = control;
        controlButtonGroups = new HashMap<String, KahinaControlButtonGroup>();
        buttonGroupOrdering = new ArrayList<String>();
    }
    
    public void addControlButtonGroup(String groupCaption)
    {
        if (controlButtonGroups.get(groupCaption) == null)
        {
            controlButtonGroups.put(groupCaption, new KahinaControlButtonGroup(groupCaption));
        }
        buttonGroupOrdering.add(groupCaption);
    }
    
    //used to add simple button definitions (not more than an icon path, a command, a tool tip and a mnemonic)
    public void addControlButton(String iconFilePath, String command, String toolTipText, String groupCaption, int mnemonic)
    {
        KahinaControlButtonGroup buttonGroup = controlButtonGroups.get(groupCaption);
        if (buttonGroup == null)
        {
            buttonGroup = new KahinaControlButtonGroup(groupCaption);
            controlButtonGroups.put(groupCaption, buttonGroup);
        }
        buttonGroup.addControlButton(iconFilePath, command, toolTipText, mnemonic);       
    }
    
    //used to fill the panel with the content defined by the controlButtons
    public void build()
    {
        this.removeAll();
        this.setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
        for (String buttonGroupID : buttonGroupOrdering)
        {
            KahinaControlButtonGroup buttonGroup = controlButtonGroups.get(buttonGroupID);
            JPanel panel = new JPanel();
            panel.setBorder(BorderFactory.createTitledBorder(buttonGroup.groupCaption));
            panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
            for (KahinaControlButton controlButton : buttonGroup.controlButtons)
            {
                JButton button = controlButton.create();
                button.addActionListener(this);
                panel.add(button);
            }
            add(panel);
        }
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String command = e.getActionCommand();
        control.processEvent(new KahinaControlEvent(command));
    }
}
