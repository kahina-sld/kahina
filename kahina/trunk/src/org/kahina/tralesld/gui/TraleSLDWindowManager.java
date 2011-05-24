package org.kahina.tralesld.gui;

import org.kahina.core.KahinaInstance;
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
    
    protected KahinaMainWindow createMainWindow(KahinaWindowManager windowManager, KahinaInstance<?, ?, ?> kahina)
	{
		return new TraleSLDMainWindow(windowManager, kahina);
	}
    
    protected KahinaMainWindow createMainWindow(KahinaWindowManager kahinaWindowManager, KahinaInstance<?, ?, ?> kahina, int winID)
	{
		return new TraleSLDMainWindow(this, kahina, winID);
	}

}
