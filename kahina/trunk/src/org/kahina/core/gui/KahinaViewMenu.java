package org.kahina.core.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaRunner;
import org.kahina.core.event.KahinaPerspectiveEvent;

public class KahinaViewMenu  extends JMenu implements ActionListener
{
	private static final long serialVersionUID = -8816851369583949953L;

	public KahinaViewMenu(KahinaInstance kahina)
    {
        super("View"); 
        
        //TODO: add check boxes for the different windows here
        
        this.addSeparator();
        
        //TODO: add functionality for opening new windows in various shapes
        
        this.addSeparator();
        
        JMenuItem loadPerspectiveItem = new JMenuItem("Load Perspective ...");
        loadPerspectiveItem.setActionCommand("loadPerspective");
        loadPerspectiveItem.addActionListener(this);
        this.add(loadPerspectiveItem);
            
        JMenuItem savePerspectiveItem = new JMenuItem("Save Perspective As...");
        savePerspectiveItem.setActionCommand("savePerspectiveAs");
        savePerspectiveItem.addActionListener(this);
        this.add(savePerspectiveItem);
        
        //TODO: add "Change Perspective" menu with all the perspectives defined for the application
    }
	
	@Override
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("loadPerspective"))
        {
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Load Perspective");
            chooser.showOpenDialog(this);
            File dataFile = chooser.getSelectedFile();
            if (dataFile != null) KahinaRunner.processEvent(new KahinaPerspectiveEvent(KahinaPerspectiveEvent.LOAD_PERSPECTIVE, dataFile));
        }
        else if (s.equals("savePerspective"))
        {
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Save Perspective As");
            chooser.showSaveDialog(this);
            File dataFile = chooser.getSelectedFile();
            if (dataFile != null)  KahinaRunner.processEvent(new KahinaPerspectiveEvent(KahinaPerspectiveEvent.SAVE_PERSPECTIVE, dataFile));
        }
    }
}
