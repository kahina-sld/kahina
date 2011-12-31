package org.kahina.qtype.gui;

import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.KahinaMainWindow;
import org.kahina.core.gui.KahinaWindowManager;

public class QTypeWindowManager extends KahinaWindowManager
{
	public QTypeWindowManager(KahinaGUI gui, KahinaController control)
	{
		super(gui, control);
	}
    
	@Override
    protected KahinaMainWindow createMainWindow(KahinaWindowManager windowManager)
	{
    	if (windowManager instanceof QTypeWindowManager)
    	{
    		return new QTypeMainWindow((QTypeWindowManager) windowManager);
    	}
    	else
    	{
    		System.err.println("FATAL ERROR: TraleSLDWindowManager could not create main window!");
    		System.err.println("             Building default window with dummy functionality instead.");
    		return new KahinaMainWindow(windowManager);
    	}
	}
    
    @Override
    protected KahinaMainWindow createMainWindow(KahinaWindowManager kahinaWindowManager, int winID)
	{
		return new QTypeMainWindow(this, winID);
	}
}
