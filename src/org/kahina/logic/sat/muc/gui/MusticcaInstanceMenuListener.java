package org.kahina.logic.sat.muc.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaProjectEvent;
import org.kahina.core.control.KahinaProjectEventType;
import org.kahina.core.control.KahinaSystemEvent;
import org.kahina.core.gui.menus.KahinaProjectMenu;
import org.kahina.logic.sat.muc.MUCInstance;

public class MusticcaInstanceMenuListener implements ActionListener
{
    MUCInstance kahina;
    MusticcaInstanceMenu menu;
    
    public MusticcaInstanceMenuListener(MUCInstance kahina, MusticcaInstanceMenu menu)
    {
        this.kahina = kahina;
        this.menu = menu;
    }
    
    @Override
    public void actionPerformed(ActionEvent e) 
    {
        String command = e.getActionCommand();
        if (command.equals("loadInstance"))
        {
            processLoadInstanceCommand();
        }
        else if (command.equals("quit"))
        {
            kahina.dispatchInstanceEvent(new KahinaSystemEvent(KahinaSystemEvent.QUIT));
        }
    }
    
    protected void processLoadInstanceCommand()
    {
        JFileChooser chooser = new JFileChooser(new File("."));
        chooser.setDialogTitle(kahina.getGUI().getNewGrammarString());
        if (chooser.showOpenDialog(menu) == JFileChooser.APPROVE_OPTION)
        {        
        	File dataFile = chooser.getSelectedFile();
        	if (dataFile != null) kahina.dispatchEvent(new KahinaProjectEvent(KahinaProjectEventType.NEW_PROJECT, dataFile, "default project"));
        }
        else
        {
        	System.err.println("Instance loading canceled by the user.");
        }
    }
}
