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
    
    protected KahinaMainWindow createMainWindow(KahinaWindowManager windowManager, KahinaController control, KahinaInstance<?, ?, ?> kahina)
	{
		return new TraleSLDMainWindow(windowManager, control, kahina);
	}
    
    protected KahinaMainWindow createMainWindow(KahinaWindowManager kahinaWindowManager, KahinaController control, KahinaInstance<?, ?, ?> kahina, int winID)
	{
		return new TraleSLDMainWindow(this, control, kahina, winID);
	}

}
