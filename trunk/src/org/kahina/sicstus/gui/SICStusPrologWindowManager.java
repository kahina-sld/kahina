package org.kahina.sicstus.gui;

import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.KahinaMainWindow;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.lp.gui.LogicProgrammingMainWindow;

public class SICStusPrologWindowManager extends KahinaWindowManager
{
	public SICStusPrologWindowManager(KahinaGUI gui, KahinaController control)
	{
		super(gui, control);
	}
    
    protected KahinaMainWindow createMainWindow(KahinaWindowManager windowManager)
	{
		return new LogicProgrammingMainWindow(windowManager);
	}
    
    protected KahinaMainWindow createMainWindow(KahinaWindowManager kahinaWindowManager, int winID)
	{
		return new LogicProgrammingMainWindow(this, winID);
	}
}
