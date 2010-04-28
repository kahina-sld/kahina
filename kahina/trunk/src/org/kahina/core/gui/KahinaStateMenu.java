package org.kahina.core.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.core.KahinaRunner;
import org.kahina.core.event.KahinaStateEvent;
import org.kahina.core.event.KahinaSystemEvent;

public class KahinaStateMenu extends JMenu implements ActionListener
{
	private static final long serialVersionUID = -3140345218228195395L;
    
    public KahinaStateMenu()
    {
        super("State");
            
        JMenuItem saveStateItem = new JMenuItem("Save...");
        saveStateItem.setActionCommand("saveState");
        saveStateItem.addActionListener(this);
        this.add(saveStateItem);
        
        JMenuItem loadStateItem = new JMenuItem("Restore...");
        loadStateItem.setActionCommand("loadState");
        loadStateItem.addActionListener(this);
        this.add(loadStateItem);
        
        this.addSeparator();
        
        JMenuItem quitItem = new JMenuItem("Quit");
        quitItem.setActionCommand("quit");
        quitItem.addActionListener(this);
        this.add(quitItem);
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("loadState"))
        {
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Restore state");
            chooser.showSaveDialog(this);
            File dataFile = chooser.getSelectedFile();
            if (dataFile != null) KahinaRunner.processEvent(new KahinaStateEvent(KahinaStateEvent.LOAD_STATE, dataFile));
        }
        else if (s.equals("saveState"))
        {
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Save current state");
            chooser.showSaveDialog(this);
            File dataFile = chooser.getSelectedFile();
            if (dataFile != null)  KahinaRunner.processEvent(new KahinaStateEvent(KahinaStateEvent.SAVE_STATE, dataFile));
        }
        else if (s.equals("quit"))
        {
            KahinaRunner.processEvent(new KahinaSystemEvent(KahinaSystemEvent.QUIT));
        }
    }
}
