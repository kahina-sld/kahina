package org.kahina.tralesld.gui;

import org.kahina.core.gui.menus.ProjectMenu;
import org.kahina.lp.gui.LogicProgrammingMainWindow;

public class TraleSLDMainWindow extends LogicProgrammingMainWindow
{
	private static final long serialVersionUID = -8044329699904664157L;

	public TraleSLDMainWindow(TraleSLDWindowManager windowManager)
	{
		super(windowManager, windowManager.getGuiControl());
	}
	
	public TraleSLDMainWindow(TraleSLDWindowManager windowManager, int winID)
	{
		super(windowManager, windowManager.getGuiControl(), winID);
	}
	
	protected void addMenusInFront()
	{
		menuBar.add(new ProjectMenu(wm.kahina));
		//TODO: define and add a good ProjectMenuListener
		menuBar.add(new TraleSLDParseMenu(wm.kahina));
		//menuBar.add(new TraleSLDWorkbenchMenu(((TraleSLDGUI) wm.gui)));
	}

}
