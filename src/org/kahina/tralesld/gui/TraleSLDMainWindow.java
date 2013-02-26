package org.kahina.tralesld.gui;

import org.kahina.core.KahinaInstance;
import org.kahina.core.gui.menus.KahinaProjectMenu;
import org.kahina.lp.gui.LogicProgrammingMainWindow;

public class TraleSLDMainWindow extends LogicProgrammingMainWindow
{
	private static final long serialVersionUID = -8044329699904664157L;

	public TraleSLDMainWindow(TraleSLDWindowManager windowManager, KahinaInstance<?, ?, ?, ?> kahina)
	{
		super(windowManager, kahina);
	}
	
	public TraleSLDMainWindow(TraleSLDWindowManager windowManager, int winID, KahinaInstance<?, ?, ?, ?> kahina)
	{
		super(windowManager, kahina, winID);
	}
	
	protected void addMenusInFront()
	{
		menuBar.add(new TraleSLDParseMenu(kahina));
		//menuBar.add(new TraleSLDWorkbenchMenu(((TraleSLDGUI) wm.gui)));
	}

}
