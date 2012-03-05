package org.kahina.lp.gui;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.gui.menus.KahinaControlPointMenu;
import org.kahina.core.gui.profiler.KahinaProfilerMenu;
import org.kahina.core.gui.windows.KahinaMainWindow;
import org.kahina.tralesld.gui.TraleSLDParseMenu;

public class LogicProgrammingMainWindow extends KahinaMainWindow
{
	private static final long serialVersionUID = -8044329699904664157L;

	public LogicProgrammingMainWindow(KahinaWindowManager windowManager, KahinaController control)
	{
		super(windowManager, control);
	}
	
	public LogicProgrammingMainWindow(KahinaWindowManager windowManager, KahinaController control, int winID)
	{
		super(windowManager, control, winID);
	}
	
	protected void addMenusBeforeHelpMenu()
	{
		menuBar.add(new KahinaControlPointMenu(control));
		menuBar.add(new KahinaProfilerMenu(control));
	}
}
