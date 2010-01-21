package org.kahina.gui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kahina.control.KahinaController;
import org.kahina.core.KahinaInstance;
import org.kahina.visual.KahinaView;

public class KahinaGUI 
{
    KahinaInstance kahina;
    KahinaController control;
    
    List<KahinaView> views;
    //values as defined in KahinaViewVisibility
    Map<KahinaView, Integer> viewVisibility;
    KahinaWindow window;
    
    public KahinaGUI(KahinaInstance kahina, KahinaController control)
    {
        this.kahina = kahina;
        this.control = control;
        
        this.views = new ArrayList<KahinaView>();
        this.viewVisibility = new HashMap<KahinaView, Integer>();
        
        window = new KahinaWindow(this, control);
        window.setVisible(true);
    }
}
