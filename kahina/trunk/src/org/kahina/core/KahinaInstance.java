package org.kahina.core;

import org.kahina.bridge.KahinaBridge;
import org.kahina.control.KahinaController;
import org.kahina.data.KahinaObject;
import org.kahina.gui.KahinaGUI;

public class KahinaInstance
{
    InformationProvider data;
    KahinaState state;
    KahinaController control;
    KahinaGUI gui;
    KahinaBridge bridge;
    
    public KahinaInstance()
    {
        data = new InformationProvider();
        state = new KahinaState();
        control = new KahinaController();
        gui = new KahinaGUI(this, control);
        bridge = new KahinaBridge();
    }
    
    public void storeInfo(String type, int infoID, KahinaObject value)
    {
        data.storeInfo(type, infoID, value);
    }
}
