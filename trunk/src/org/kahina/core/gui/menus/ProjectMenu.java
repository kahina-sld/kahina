package org.kahina.core.gui.menus;

import java.awt.event.ActionListener;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.core.KahinaInstance;

public class ProjectMenu extends JMenu
{
	public JMenuItem newProjectItem;
	public JMenuItem loadProjectItem;
	public JMenuItem saveProjectItem;
	
	public ProjectMenu(KahinaInstance<?, ?, ?, ?> kahina)
	{
		super("Project");
		
		//default action listener
		ProjectMenuListener defaultListener = new ProjectMenuListener(kahina, this);
		
		newProjectItem = new JMenuItem("New project");
		newProjectItem.setActionCommand("newProject");
		newProjectItem.addActionListener(defaultListener);
		this.add(newProjectItem);
		
		this.addSeparator();
		
		loadProjectItem = new JMenuItem("Load project");
		loadProjectItem.setActionCommand("loadProject");
		loadProjectItem.addActionListener(defaultListener);
		this.add(loadProjectItem);
		
		saveProjectItem = new JMenuItem("Save project");
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
	public void setActionListener(ProjectMenuListener listener)
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
