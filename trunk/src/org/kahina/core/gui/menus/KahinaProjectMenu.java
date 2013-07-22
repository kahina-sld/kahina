package org.kahina.core.gui.menus;

import java.awt.event.ActionListener;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.project.KahinaProject;

public class KahinaProjectMenu extends JMenu
{

	private static final long serialVersionUID = -5797303674332544798L;
	
	public JMenuItem newProjectItem;
	public JMenu openProjectMenu;
	public JMenu recentProjectsMenu;
	public JMenu predefinedProjectsMenu;
	public JMenuItem loadProjectItem;
	public JMenuItem saveProjectItem;
	
	public KahinaProjectMenu(KahinaInstance<?, ?, ?, ?> kahina)
	{
		super("Project");
		
		//default action listener
		KahinaProjectMenuListener defaultListener = new KahinaProjectMenuListener(kahina, this);
		
		newProjectItem = new JMenuItem("New Project");
		newProjectItem.setActionCommand("newProject");
		newProjectItem.addActionListener(defaultListener);
		this.add(newProjectItem);
		
		this.addSeparator();
		
		openProjectMenu = new JMenu("Open Project");
		
		recentProjectsMenu = new JMenu("Recent Projects");
        int count = 0;
        for (KahinaProject p : kahina.recentProjects)
        {
            JMenuItem recentProjectItem = new JMenuItem(p.getName());
            recentProjectItem.setActionCommand("loadRecentProject:" + count++);
            recentProjectItem.addActionListener(defaultListener);
            recentProjectsMenu.add(recentProjectItem);
        }
		openProjectMenu.add(recentProjectsMenu);
		
	    predefinedProjectsMenu = new JMenu("Predefined Projects");
	    count = 0;
        for (KahinaProject p : kahina.defaultProjects)
        {
            JMenuItem defaultProjectItem = new JMenuItem(p.getName());
            defaultProjectItem.setActionCommand("loadDefaultProject:" + count++);
            defaultProjectItem.addActionListener(defaultListener);
            predefinedProjectsMenu.add(defaultProjectItem);
        }
	    openProjectMenu.add(predefinedProjectsMenu);
		
		loadProjectItem = new JMenuItem("Load Project...");
		loadProjectItem.setActionCommand("loadProject");
		loadProjectItem.addActionListener(defaultListener);
		openProjectMenu.add(loadProjectItem);
		
		this.add(openProjectMenu);
		
		saveProjectItem = new JMenuItem("Save Project As...");
		saveProjectItem.setActionCommand("saveProject");
		saveProjectItem.addActionListener(defaultListener);
		this.add(saveProjectItem);
		
        this.addSeparator();
        
        JMenuItem quitItem = new JMenuItem("Quit");
        quitItem.setActionCommand("quit");
        quitItem.addActionListener(defaultListener);
        this.add(quitItem);
	}
	
	/**
	 * Sets a specialized menu listener for this menu.
	 * An application should call this to define functionality for the menu.
	 * The argument must inherit from ProjectMenuListener.
	 * @param listener the listener to process the menu's ActionEvents
	 */
	public void setActionListener(KahinaProjectMenuListener listener)
	{
		for (ActionListener oldListener : newProjectItem.getActionListeners())
		{
			newProjectItem.removeActionListener(oldListener);
		}
		newProjectItem.addActionListener(listener);
		
		for (ActionListener oldListener : loadProjectItem.getActionListeners())
		{
			loadProjectItem.removeActionListener(oldListener);
		}
		loadProjectItem.addActionListener(listener);
		
		for (ActionListener oldListener : saveProjectItem.getActionListeners())
		{
			saveProjectItem.removeActionListener(oldListener);
		}
		saveProjectItem.addActionListener(listener);
	}
}
