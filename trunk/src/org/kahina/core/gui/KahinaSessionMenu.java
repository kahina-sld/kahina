package org.kahina.core.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.core.KahinaRunner;
import org.kahina.core.event.KahinaSessionEvent;
import org.kahina.core.event.KahinaSystemEvent;

public class KahinaSessionMenu extends JMenu implements ActionListener
{
	private static final long serialVersionUID = -3140345218228195395L;
    
    public KahinaSessionMenu()
    {
        super("Session");
        
        JMenuItem loadSessionItem = new JMenuItem("Load...");
        loadSessionItem.setActionCommand("loadSession");
        loadSessionItem.addActionListener(this);
        this.add(loadSessionItem);
            
        JMenuItem saveSessionItem = new JMenuItem("Save As...");
        saveSessionItem.setActionCommand("saveSessionAs");
        saveSessionItem.addActionListener(this);
        this.add(saveSessionItem);
        
        this.addSeparator();
        
        JMenuItem quitItem = new JMenuItem("Quit");
        quitItem.setActionCommand("quit");
        quitItem.addActionListener(this);
        this.add(quitItem);
    }
    
    @Override
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("loadSession"))
        {
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Load Session");
            chooser.showOpenDialog(this);
            File dataFile = chooser.getSelectedFile();
            if (dataFile != null) KahinaRunner.processEvent(new KahinaSessionEvent(KahinaSessionEvent.LOAD_SESSION, dataFile));
        }
        else if (s.equals("saveSessionAs"))
        {
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Save Session As");
            chooser.showSaveDialog(this);
            File dataFile = chooser.getSelectedFile();
            if (dataFile != null)  KahinaRunner.processEvent(new KahinaSessionEvent(KahinaSessionEvent.SAVE_SESSION, dataFile));
        }
        else if (s.equals("quit"))
        {
            KahinaRunner.processEvent(new KahinaSystemEvent(KahinaSystemEvent.QUIT));
        }
    }
}
