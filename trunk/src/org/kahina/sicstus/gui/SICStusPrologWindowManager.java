package org.kahina.sicstus.gui;

import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.gui.windows.KahinaMainWindow;
import org.kahina.lp.gui.LogicProgrammingMainWindow;
import org.kahina.sicstus.SICStusPrologDebuggerInstance;

public class SICStusPrologWindowManager extends KahinaWindowManager
{
	public SICStusPrologWindowManager(SICStusPrologDebuggerInstance kahina)
	{
		super(kahina, false);
	}
    
    protected KahinaMainWindow createMainWindow(KahinaWindowManager windowManager)
	{
		return new LogicProgrammingMainWindow(windowManager, kahina);
	}
    
    protected KahinaMainWindow createMainWindow(KahinaWindowManager windowManager, int winID)
	{
		return new LogicProgrammingMainWindow(this, kahina, winID);
	}
}
