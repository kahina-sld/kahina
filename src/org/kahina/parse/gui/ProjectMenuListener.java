package org.kahina.parse.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * A default listener for the project menu, does not do anything by default.
 * Should be specialized by applications.
 * Instead of overriding the actionPerformed method, it might be more straightforward
 * to override the provided convenience methods.
 * 
 * @author jd
 *
 */
public class ProjectMenuListener implements ActionListener
{
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
		else if (command.equals("closeProject"))
		{
			processCloseProjectCommand();
		}
	}
	
	protected void processNewProjectCommand()
	{
		
	}
	
	protected void processLoadProjectCommand()
	{
		
	}
	
	protected void processSaveProjectCommand()
	{
		
	}
	
	protected void processCloseProjectCommand()
	{
		
	}

}
