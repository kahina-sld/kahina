package org.kahina.core.gui.menus;

import java.awt.event.ActionListener;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.core.KahinaInstance;

public class KahinaProjectMenu extends JMenu
{
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
		openProjectMenu.add(recentProjectsMenu);
		
	    predefinedProjectsMenu = new JMenu("Predefined Projects");
	    openProjectMenu.add(predefinedProjectsMenu);
		
		loadProjectItem = new JMenuItem("Load Project ...");
		loadProjectItem.setActionCommand("loadProject");
		loadProjectItem.addActionListener(defaultListener);
		openProjectMenu.add(loadProjectItem);
		
		this.add(openProjectMenu);
		
		saveProjectItem = new JMenuItem("Save Project As ...");
		saveProjectItem.setActionCommand("saveProject");
		saveProjectItem.addActionListener(defaultListener);
		this.add(saveProjectItem);
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
