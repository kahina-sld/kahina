package org.kahina.tulipa.gui;

import java.awt.Color;

import org.kahina.core.KahinaStep;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.KahinaViewIntegrationType;
import org.kahina.core.visual.chart.KahinaChartView;
import org.kahina.core.visual.dag.KahinaDAGView;
import org.kahina.tralesld.TraleSLDInstance;
import org.kahina.tralesld.TraleSLDStepType;
import org.kahina.tralesld.data.chart.TraleSLDChartEdgeStatus;
import org.kahina.tralesld.visual.chart.TraleSLDChartEdgeDisplayDecider;
import org.kahina.tulipa.TulipaInstance;

public class TulipaGUI extends KahinaGUI
{
    private TulipaInstance instance;
    
    protected KahinaDAGView mainDAGView;
    
    public TulipaGUI(Class<? extends KahinaStep> stepType, TulipaInstance instance)
    {
        super(stepType, instance);
        this.instance = instance;      
        
        mainDAGView = new KahinaDAGView();
        mainDAGView.setTitle("Item Graph");
        views.add(mainDAGView);
        livingViews.add(mainDAGView); 
        varNameToView.put("dag", mainDAGView);

        mainTreeView.setStatusColorEncoding(TraleSLDStepType.FINISHED, new Color(102,51,153));
        mainTreeView.setStatusColorEncoding(TraleSLDStepType.BLOCKED, Color.BLACK);
        //TODO: build font color customization facilities into TreeView
        //mainTreeView.setStatusFontColorEncoding(TraleSLDStepType.BLOCKED, Color.BLACK);
    }
    
    protected void displayMainViews()
    {
        //TODO: remove the standard tree view manually or do not make it part of a standard Kahina
        super.displayMainViews();
        mainDAGView.display(instance.getState().getDAG());  
    }
    
    public void prepare()
    {
        super.prepare();
        getWindowForVarName("dag").setSize(800, 500);
        getWindowForVarName("dag").setLocation(0, 150);
    }
}
