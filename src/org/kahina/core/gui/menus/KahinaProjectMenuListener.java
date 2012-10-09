package org.kahina.core.gui.menus;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaProjectEvent;
import org.kahina.core.control.KahinaProjectEventType;
import org.kahina.core.control.KahinaSessionEvent;
import org.kahina.core.control.KahinaSystemEvent;
import org.kahina.core.gui.event.KahinaPerspectiveEvent;

/**
 * A default listener for the project menu, only creates and dispatches KahinaProjectEvents by default.
 * Can be specialized by applications. Instead of overriding the actionPerformed method, 
 * it might be more straightforward to override the provided convenience methods.
 * 
 * @author jd
 *
 */
public class KahinaProjectMenuListener implements ActionListener
{
    KahinaInstance<?, ?, ?, ?> kahina;
    KahinaProjectMenu menu;
    
    public KahinaProjectMenuListener(KahinaInstance<?,?,?,?> kahina, KahinaProjectMenu menu)
    {
        this.kahina = kahina;
        this.menu = menu;
    }
    
	@Override
	public void actionPerformed(ActionEvent e) 
	{
		String command = e.getActionCommand();
		if (command.equals("newProject"))
		{
			processNewProjectCommand();
		}
		else if (command.equals("loadProject"))
		{
			processLoadProjectCommand();
		}
        else if (command.startsWith("loadRecentProject:"))
        {
            int counter = Integer.parseInt(command.substring(18));
            kahina.processEvent(new KahinaProjectEvent(KahinaProjectEventType.LOAD_RECENT_PROJECT, counter));
        }
        else if (command.startsWith("loadDefaultProject:"))
        {
            int counter = Integer.parseInt(command.substring(19));
            kahina.processEvent(new KahinaProjectEvent(KahinaProjectEventType.LOAD_DEFAULT_PROJECT, counter));
        }
		else if (command.equals("saveProject"))
		{
			processSaveProjectCommand();
		}
        else if (command.equals("quit"))
        {
            kahina.dispatchInstanceEvent(new KahinaSystemEvent(KahinaSystemEvent.QUIT));
        }
	}
	
	protected void processNewProjectCommand()
	{
	    String name = (String) JOptionPane.showInputDialog(menu,
                "Enter a name for the project.",
                "New Project",
                JOptionPane.PLAIN_MESSAGE);
        if (name.length() > 0)
        {
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("New Project - Select Program File");
            chooser.showOpenDialog(menu);
            File dataFile = chooser.getSelectedFile();
            if (dataFile != null) kahina.dispatchEvent(new KahinaProjectEvent(KahinaProjectEventType.NEW_PROJECT, dataFile, name));
        }
        else
        {
            JOptionPane.showMessageDialog(menu, "Empty string is not a valid name!");
        }
	}
	
	protected void processLoadProjectCommand()
	{
        JFileChooser chooser = new JFileChooser(new File("."));
        chooser.setDialogTitle("Load Project");
        chooser.showOpenDialog(menu);
        File dataFile = chooser.getSelectedFile();
        if (dataFile != null) kahina.dispatchEvent(new KahinaProjectEvent(KahinaProjectEventType.LOAD_PROJECT, dataFile, ""));
	}
	
	protected void processSaveProjectCommand()
	{
        JFileChooser chooser = new JFileChooser(new File("."));
        chooser.setDialogTitle("Save Project");
        chooser.showSaveDialog(menu);
        File dataFile = chooser.getSelectedFile();
        if (dataFile != null) kahina.dispatchEvent(new KahinaProjectEvent(KahinaProjectEventType.SAVE_PROJECT, dataFile, ""));
	}
}
