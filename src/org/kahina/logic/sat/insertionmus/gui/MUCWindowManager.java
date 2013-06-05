package org.kahina.logic.sat.insertionmus.gui;

import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.gui.windows.KahinaMainWindow;
import org.kahina.logic.sat.insertionmus.MUCInstance;

public class MUCWindowManager extends KahinaWindowManager
{
    MUCInstance kahina;
    
    public MUCWindowManager(MUCInstance kahina)
    {
        super(kahina);
        this.kahina = kahina;
    }

    protected KahinaMainWindow createMainWindow(MUCWindowManager windowManager, MUCInstance kahina)
    {
        return new MUCMainWindow(windowManager, kahina);
    }
    
    @Override
    protected KahinaMainWindow createMainWindow(KahinaWindowManager kahinaWindowManager, int winID)
    {
        return new MUCMainWindow(this, winID, kahina);
    }
}
