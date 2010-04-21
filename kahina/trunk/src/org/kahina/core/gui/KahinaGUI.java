package org.kahina.core.gui;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.kahina.core.KahinaException;
import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaRunner;
import org.kahina.core.KahinaStep;
import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.tree.KahinaTreeView;

public class KahinaGUI implements KahinaListener
{
    KahinaInstance kahina;
    KahinaController control;
    
    KahinaControlPanel controlPanel;
    
    protected KahinaTreeView mainTreeView;
    
    List<KahinaView> views;
    //values as defined in KahinaViewVisibility
    Map<KahinaView, Integer> viewVisibility;
    KahinaWindow window;
    
    Set<KahinaView> livingViews;
    
    Map<Field, KahinaView> fieldToView;
    
    Class<? extends KahinaStep> stepType;
    
    public KahinaGUI(Class<? extends KahinaStep> stepType, KahinaInstance kahina, KahinaController control) 
    {
        System.err.println("creating Kahina GUI...");
        this.stepType = stepType;
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
        mainTreeView.setTitle("Control flow tree");
        control.registerListener("update", mainTreeView);
        views.add(mainTreeView);
        livingViews.add(mainTreeView);
    }
    
    /**
     * overwrite this to specify a non-standard mapping from step properties to views
     * @param stepType
     */
    protected void fillFieldToView(Class<? extends KahinaStep> stepType)
    {
        System.err.println("Generating views for step fields:");
        for (Field field : stepType.getFields())
        {    
            System.err.println("\tfield: " + field.getName() + "\n\t\tclass: " + field.getType());
            if (KahinaObject.class.isAssignableFrom(field.getType()))
            {
                KahinaView<?> newView = KahinaViewRegistry.generateViewFor(field.getType());
                System.err.println("\t\tview: " + newView);
                newView.setTitle("Step information: " + field.getName());
                fieldToView.put(field, newView);
                views.add(newView);
                livingViews.add(newView);
            }
        }
    }
    
    public KahinaControlPanel getControlPanel()
    {
        return controlPanel;
    }
    
    public void buildAndShow()
    {
        mainTreeView.display(kahina.getState().getStepTree());   
        mainTreeView.displaySecondaryTree(kahina.getState().getSecondaryStepTree()); 
        window = new KahinaWindow(this, control);
    }
    

    @SuppressWarnings("unchecked")
    public void displayStepContent(int stepID)
    {
        KahinaStep step = KahinaRunner.getDataManager().retrieve(stepType, stepID);
        for (Field field : fieldToView.keySet())
        {    
            try
            {
                fieldToView.get(field).display((KahinaObject) field.get(step));
            }
            catch (IllegalAccessException e)
            {
                throw new KahinaException("Problem in displaying step content!", e);
            }
        }
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
        if (e.getView() == null || livingViews.contains(e.getView()))
        {
            int selectedStep = e.getSelectedStep();
            control.processEvent(new KahinaUpdateEvent(selectedStep));
            displayStepContent(selectedStep);
            control.processEvent(new KahinaRedrawEvent());
        }
    }
}
