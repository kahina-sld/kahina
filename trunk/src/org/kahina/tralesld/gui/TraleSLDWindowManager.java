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
    	if (windowManager instanceof TraleSLDWindowManager)
    	{
    		return new TraleSLDMainWindow((TraleSLDWindowManager) windowManager);
    	}
    	else
    	{
    		System.err.println("FATAL ERROR: TraleSLDWindowManager could not create main window!");
    		System.err.println("             Building default window with dummy functionality instead.");
    		return new KahinaMainWindow(windowManager);
    	}
	}
    
    protected KahinaMainWindow createMainWindow(KahinaWindowManager kahinaWindowManager, int winID)
	{
		return new TraleSLDMainWindow(this, winID);
	}
}
