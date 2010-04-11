package org.kahina.core.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.core.control.KahinaController;
import org.kahina.core.control.event.KahinaDialogEvent;

public class KahinaHelpMenu extends JMenu implements ActionListener
{
    KahinaController control;
    
    public KahinaHelpMenu(KahinaController control)
    {
        super("?");
        this.control = control;
            
        JMenuItem helpItem = new JMenuItem("Help");
        helpItem.setActionCommand("help");
        helpItem.addActionListener(this);
        this.add(helpItem);
        
        this.addSeparator();
        
        JMenuItem aboutItem = new JMenuItem("About...");
        aboutItem.setActionCommand("about");
        aboutItem.addActionListener(this);
        this.add(aboutItem);
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("help"))
        {
            control.processEvent(new KahinaDialogEvent(KahinaDialogEvent.HELP));
        }
        else if (s.equals("about"))
        {
            control.processEvent(new KahinaDialogEvent(KahinaDialogEvent.ABOUT));
        }
    }   
}
