package org.kahina.logic.sat.freemuc.gui;

import org.kahina.core.KahinaInstance;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.gui.windows.KahinaMainWindow;
import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.muc.gui.MUCMainWindow;
import org.kahina.logic.sat.muc.gui.MUCWindowManager;

public class FreeMUCWindowManager extends KahinaWindowManager
{
    public FreeMUCWindowManager(KahinaInstance<?, ?, ?, ?> kahina)
    {
        super(kahina);
    }

    protected KahinaMainWindow createMainWindow(KahinaWindowManager windowManager, MUCInstance kahina)
    {
        if (windowManager instanceof MUCWindowManager)
        {
            return new MUCMainWindow((MUCWindowManager) windowManager, kahina);
        }
        else
        {
            System.err.println("FATAL ERROR: FreeMUCWindowManager could not create main window!");
            System.err.println("             Building default window with dummy functionality instead.");
            return new KahinaMainWindow(windowManager, kahina);
        }
    }
    
    @Override
    protected KahinaMainWindow createMainWindow(KahinaWindowManager kahinaWindowManager, int winID)
    {
        return new FreeMUCMainWindow(this, winID, kahina);
    }
}
