package org.kahina.tralesld.gui;

import org.kahina.lp.gui.LogicProgrammingMainWindow;
import org.kahina.parse.gui.ProjectMenu;

public class TraleSLDMainWindow extends LogicProgrammingMainWindow
{
	private static final long serialVersionUID = -8044329699904664157L;

	public TraleSLDMainWindow(TraleSLDWindowManager windowManager)
	{
		super(windowManager);
	}
	
	public TraleSLDMainWindow(TraleSLDWindowManager windowManager, int winID)
	{
		super(windowManager, winID);
	}
	
	protected void addMenusInFront()
	{
		menuBar.add(new ProjectMenu(wm.gui.getKahinaInstance()));
		//TODO: define and add a good ProjectMenuListener
		menuBar.add(new TraleSLDParseMenu(wm.gui.getKahinaInstance()));
		//menuBar.add(new TraleSLDWorkbenchMenu(((TraleSLDGUI) wm.gui)));
	}

}
