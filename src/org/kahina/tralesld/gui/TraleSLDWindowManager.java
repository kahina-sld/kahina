package org.kahina.tralesld.gui;

import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.gui.windows.KahinaMainWindow;
import org.kahina.tralesld.TraleSLDInstance;

public class TraleSLDWindowManager extends KahinaWindowManager
{
	public TraleSLDWindowManager(TraleSLDInstance kahina)
	{
		super(kahina, false);
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
    		return new KahinaMainWindow(windowManager, windowManager.getGuiControl());
    	}
	}
    
    protected KahinaMainWindow createMainWindow(KahinaWindowManager kahinaWindowManager, int winID)
	{
		return new TraleSLDMainWindow(this, winID);
	}
}
