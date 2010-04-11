package org.kahina.core.gui;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaStep;
import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.control.event.KahinaEvent;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.tree.KahinaTreeView;

public class KahinaGUI implements KahinaListener
{
    KahinaInstance kahina;
    KahinaController control;
    
    KahinaControlPanel controlPanel;
    
    KahinaTreeView mainTreeView;
    
    List<KahinaView> views;
    //values as defined in KahinaViewVisibility
    Map<KahinaView, Integer> viewVisibility;
    KahinaWindow window;
    
    Set<KahinaView> livingViews;
    
    Map<Field, KahinaView> fieldToView;
    
    public KahinaGUI(Class<? extends KahinaStep> stepType, KahinaInstance kahina, KahinaController control) 
    {
        this.kahina = kahina;
        this.control = control;
        control.registerListener("select", this);
        
        this.controlPanel = new KahinaControlPanel(control);
        
        this.views = new ArrayList<KahinaView>();
        this.viewVisibility = new HashMap<KahinaView, Integer>();
        
        this.livingViews = new HashSet<KahinaView>();
        this.fieldToView = new HashMap<Field, KahinaView>();
        fillFieldToView(stepType);
        
        mainTreeView = new KahinaTreeView();
        control.registerListener("update", mainTreeView);
        mainTreeView.display(kahina.getState().getStepTree());
        views.add(mainTreeView);
    }
    
    /**
     * overwrite this to specify a non-standard mapping from step properties to views
     * @param stepType
     */
    protected void fillFieldToView(Class<? extends KahinaStep> stepType)
    {
        for (Field field : stepType.getFields())
        {
            if (KahinaObject.class.isAssignableFrom(field.getClass()))
            {
                //TODO: get corresponding views from registry
                KahinaView newView = KahinaViewRegistry.generateViewFor(stepType);
                fieldToView.put(field, newView);
                views.add(newView);
            }
        }
    }
    
    public KahinaControlPanel getControlPanel()
    {
        return controlPanel;
    }
    
    public void buildAndShow()
    {
        window = new KahinaWindow(this, control);
    }
    
    public void displayStepContent(int stepID)
    {
        
    }
    
    public void processEvent(KahinaEvent e)
    {
        if (e instanceof KahinaSelectionEvent)
        {
            processEvent((KahinaSelectionEvent) e);
        }
    }
    
    public void processEvent(KahinaSelectionEvent e)
    {
        if (livingViews.contains(e.getView()))
        {
            int selectedStep = e.getSelectedStep();
            control.processEvent(new KahinaUpdateEvent(selectedStep));
            displayStepContent(selectedStep);
        }
    }
}
