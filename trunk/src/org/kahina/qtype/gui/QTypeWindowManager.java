package org.kahina.qtype.gui;

import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.gui.windows.KahinaMainWindow;
import org.kahina.qtype.QTypeDebuggerInstance;

public class QTypeWindowManager extends KahinaWindowManager
{
	public QTypeWindowManager(QTypeDebuggerInstance kahina)
	{
		super(kahina, false);
	}
    
	@Override
    protected KahinaMainWindow createMainWindow(KahinaWindowManager windowManager)
	{
    	if (windowManager instanceof QTypeWindowManager)
    	{
    		return new QTypeMainWindow((QTypeWindowManager) windowManager, windowManager.getGuiControl());
    	}
    	else
    	{
    		System.err.println("FATAL ERROR: TraleSLDWindowManager could not create main window!");
    		System.err.println("             Building default window with dummy functionality instead.");
    		return new KahinaMainWindow(windowManager, windowManager.getGuiControl());
    	}
	}
    
    @Override
    protected KahinaMainWindow createMainWindow(KahinaWindowManager kahinaWindowManager, int winID)
	{
		return new QTypeMainWindow(this, kahinaWindowManager.getGuiControl(), winID);
	}
}
