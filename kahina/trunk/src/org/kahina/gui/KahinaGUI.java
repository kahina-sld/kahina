package org.kahina.gui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kahina.control.KahinaController;
import org.kahina.core.KahinaInstance;
import org.kahina.visual.KahinaView;
import org.kahina.visual.tree.KahinaTreeView;

public class KahinaGUI 
{
    KahinaInstance kahina;
    KahinaController control;
    
    KahinaControlPanel controlPanel;
    
    KahinaTreeView mainTreeView;
    
    List<KahinaView> views;
    //values as defined in KahinaViewVisibility
    Map<KahinaView, Integer> viewVisibility;
    KahinaWindow window;
    
    public KahinaGUI(KahinaInstance kahina, KahinaController control)
    {
        this.kahina = kahina;
        this.control = control;
        
        this.controlPanel = new KahinaControlPanel(control);
        
        this.views = new ArrayList<KahinaView>();
        this.viewVisibility = new HashMap<KahinaView, Integer>();
        
        mainTreeView = new KahinaTreeView();
        mainTreeView.display(kahina.getState().getStepTree());
        views.add(mainTreeView);
    }
    
    public KahinaControlPanel getControlPanel()
    {
        return controlPanel;
    }
    
    public void buildAndShow()
    {
        window = new KahinaWindow(this, control);
    }
}
