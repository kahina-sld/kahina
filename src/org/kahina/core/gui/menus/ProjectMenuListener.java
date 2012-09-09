package org.kahina.core.gui.menus;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JFileChooser;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaProjectEvent;
import org.kahina.core.control.KahinaProjectEventType;
import org.kahina.core.control.KahinaSessionEvent;

/**
 * A default listener for the project menu, only creates and dispatches KahinaProjectEvents by default.
 * Can be specialized by applications. Instead of overriding the actionPerformed method, 
 * it might be more straightforward to override the provided convenience methods.
 * 
 * @author jd
 *
 */
public class ProjectMenuListener implements ActionListener
{
    KahinaInstance<?, ?, ?, ?> kahina;
    ProjectMenu menu;
    
    public ProjectMenuListener(KahinaInstance<?,?,?,?> kahina, ProjectMenu menu)
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
		else if (command.equals("saveProject"))
		{
			processSaveProjectCommand();
		}
	}
	
	protected void processNewProjectCommand()
	{
        JFileChooser chooser = new JFileChooser(new File("."));
        chooser.setDialogTitle("New Project - Select Program File");
        chooser.showOpenDialog(menu);
        File dataFile = chooser.getSelectedFile();
        if (dataFile != null) kahina.dispatchEvent(new KahinaProjectEvent(KahinaProjectEventType.NEW_PROJECT, dataFile));
	}
	
	protected void processLoadProjectCommand()
	{
        JFileChooser chooser = new JFileChooser(new File("."));
        chooser.setDialogTitle("Load Project");
        chooser.showOpenDialog(menu);
        File dataFile = chooser.getSelectedFile();
        if (dataFile != null) kahina.dispatchEvent(new KahinaProjectEvent(KahinaProjectEventType.LOAD_PROJECT, dataFile));
	}
	
	protected void processSaveProjectCommand()
	{
        JFileChooser chooser = new JFileChooser(new File("."));
        chooser.setDialogTitle("Save Project");
        chooser.showSaveDialog(menu);
        File dataFile = chooser.getSelectedFile();
        if (dataFile != null) kahina.dispatchEvent(new KahinaProjectEvent(KahinaProjectEventType.SAVE_PROJECT, dataFile));
	}
}
