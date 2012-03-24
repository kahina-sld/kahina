package org.kahina.lp.gui;

import org.kahina.core.KahinaInstance;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.gui.menus.KahinaControlPointMenu;
import org.kahina.core.gui.profiler.KahinaProfilerMenu;
import org.kahina.core.gui.windows.KahinaMainWindow;

public class LogicProgrammingMainWindow extends KahinaMainWindow
{
	private static final long serialVersionUID = -8044329699904664157L;

	public LogicProgrammingMainWindow(KahinaWindowManager windowManager, KahinaInstance<?, ?, ?> kahina)
	{
		super(windowManager, kahina);
	}
	
	public LogicProgrammingMainWindow(KahinaWindowManager windowManager, KahinaInstance<?, ?, ?> kahina, int winID)
	{
		super(windowManager, kahina, winID);
	}
	
	protected void addMenusBeforeHelpMenu()
	{
		menuBar.add(new KahinaControlPointMenu(kahina));
		menuBar.add(new KahinaProfilerMenu(kahina));
	}
}
