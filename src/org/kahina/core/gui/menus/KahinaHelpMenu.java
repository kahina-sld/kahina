package org.kahina.core.gui.menus;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaDialogEvent;

public class KahinaHelpMenu extends JMenu implements ActionListener
{
	private static final long serialVersionUID = -8825991093423631389L;
    private KahinaController guiControl;

	public KahinaHelpMenu(KahinaController guiControl)
    {
        super("?");
        this.guiControl = guiControl;
            
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
            guiControl.processEvent(new KahinaDialogEvent(KahinaDialogEvent.HELP));
        }
        else if (s.equals("about"))
        {
            guiControl.processEvent(new KahinaDialogEvent(KahinaDialogEvent.ABOUT));
        }
    }   
}
