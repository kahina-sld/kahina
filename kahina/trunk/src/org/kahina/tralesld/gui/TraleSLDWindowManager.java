package org.kahina.tralesld.gui;

import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.KahinaMainWindow;
import org.kahina.core.gui.KahinaWindowManager;

public class TraleSLDWindowManager extends KahinaWindowManager
{
	public TraleSLDWindowManager(KahinaGUI gui, KahinaController control)
	{
		super(gui, control);
	}
    
    protected KahinaMainWindow createMainWindow(KahinaWindowManager windowManager)
	{
		return new TraleSLDMainWindow(windowManager);
	}
    
    protected KahinaMainWindow createMainWindow(KahinaWindowManager kahinaWindowManager, int winID)
	{
		return new TraleSLDMainWindow(this, winID);
	}

}
