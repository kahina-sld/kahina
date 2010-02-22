package org.kahina.core;

import org.kahina.bridge.KahinaBridge;
import org.kahina.control.KahinaController;
import org.kahina.data.DataManager;
import org.kahina.gui.KahinaGUI;

public class KahinaInstance
{
    DataManager dataManager;
    KahinaState state;
    KahinaController controller;
    KahinaGUI gui;
    KahinaBridge bridge;
    
    public KahinaInstance(DataManager dataManager)
    {
    	this.dataManager = dataManager;
        state = new KahinaState();
        controller = new KahinaController();
        gui = new KahinaGUI(this, controller);
        bridge = new KahinaBridge();
    }

    public KahinaController getController()
    {
        return controller;
    }

    public KahinaGUI getGUI()
    {
        return gui;
    }
}
