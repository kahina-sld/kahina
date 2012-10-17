package org.kahina.logic.sat.freemuc.gui;

import org.kahina.core.KahinaInstance;
import org.kahina.core.gui.windows.KahinaMainWindow;

public class FreeMUCMainWindow extends KahinaMainWindow
{
    public FreeMUCMainWindow(FreeMUCWindowManager windowManager, KahinaInstance<?, ?, ?, ?> kahina)
    {
        super(windowManager, kahina);
    }
    
    public FreeMUCMainWindow(FreeMUCWindowManager windowManager, int winID, KahinaInstance<?, ?, ?, ?> kahina)
    {
        super(windowManager, kahina, winID);
    }

    protected void addMenusInFront()
    {
        menuBar.add(new FreeMUCFileMenu(kahina));
    }
}
