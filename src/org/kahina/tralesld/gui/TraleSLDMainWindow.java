package org.kahina.tralesld.gui;

import org.kahina.lp.gui.LogicProgrammingMainWindow;

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
	
	protected void addAdditionalMenus()
	{
		menuBar.add(new TraleSLDParseMenu(wm.gui.getKahinaInstance()));
		//menuBar.add(new TraleSLDWorkbenchMenu(((TraleSLDGUI) wm.gui)));
		super.addAdditionalMenus();
	}

}
