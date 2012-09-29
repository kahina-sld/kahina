package org.kahina.core.gui.menus;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaSessionEvent;
import org.kahina.core.control.KahinaSystemEvent;

public class KahinaSessionMenu extends JMenu implements ActionListener
{
	private static final long serialVersionUID = -3140345218228195395L;
	
	KahinaInstance<?,?,?,?> kahina;
    
    public KahinaSessionMenu(KahinaInstance<?,?,?,?> kahina)
    {
        super("Session");
        this.kahina = kahina;
        
        JMenuItem loadSessionItem = new JMenuItem("Load...");
        loadSessionItem.setActionCommand("loadSession");
        loadSessionItem.addActionListener(this);
        //TODO: reliably re-establish the saved state functionality
        loadSessionItem.setEnabled(false);
        this.add(loadSessionItem);
            
        JMenuItem saveSessionItem = new JMenuItem("Save As...");
        saveSessionItem.setActionCommand("saveSessionAs");
        saveSessionItem.addActionListener(this);
        //TODO: reliably re-establish the saved state functionality
        saveSessionItem.setEnabled(false);
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
            if (dataFile != null) kahina.dispatchInstanceEvent(new KahinaSessionEvent(KahinaSessionEvent.LOAD_SESSION, dataFile));
        }
        else if (s.equals("saveSessionAs"))
        {
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Save Session As");
            chooser.showSaveDialog(this);
            File dataFile = chooser.getSelectedFile();
            if (dataFile != null)  kahina.dispatchInstanceEvent(new KahinaSessionEvent(KahinaSessionEvent.SAVE_SESSION, dataFile));
        }
        else if (s.equals("quit"))
        {
            kahina.dispatchInstanceEvent(new KahinaSystemEvent(KahinaSystemEvent.QUIT));
        }
    }
}
