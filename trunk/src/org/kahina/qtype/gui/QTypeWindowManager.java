package org.kahina.qtype.gui;

import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.gui.windows.KahinaMainWindow;
import org.kahina.qtype.QTypeDebuggerInstance;

public class QTypeWindowManager extends KahinaWindowManager
{
	public QTypeWindowManager(QTypeDebuggerInstance kahina)
	{
		super(kahina);
	}
    
	@Override
    protected KahinaMainWindow createMainWindow(KahinaWindowManager windowManager)
	{
    	if (windowManager instanceof QTypeWindowManager)
    	{
    		return new QTypeMainWindow((QTypeWindowManager) windowManager, kahina);
    	}
    	else
    	{
    		System.err.println("FATAL ERROR: QTypeWindowManager could not create main window!");
    		System.err.println("             Building default window with dummy functionality instead.");
    		return new KahinaMainWindow(windowManager, kahina);
    	}
	}
    
    @Override
    protected KahinaMainWindow createMainWindow(KahinaWindowManager kahinaWindowManager, int winID)
	{
		return new QTypeMainWindow(this, kahina, winID);
	}
}
