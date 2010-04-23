package org.kahina.tralesld.gui;

import java.awt.Color;

import org.kahina.core.KahinaStep;
import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.chart.KahinaChartView;
import org.kahina.lp.gui.LogicProgrammingGUI;
import org.kahina.tralesld.TraleSLDInstance;
import org.kahina.tralesld.TraleSLDStepType;

public class TraleSLDGUI extends LogicProgrammingGUI
{
	private TraleSLDInstance instance;
	
	protected KahinaChartView mainChartView;
	
    public TraleSLDGUI(Class<? extends KahinaStep> stepType, TraleSLDInstance instance, KahinaController control)
    {
        super(stepType, instance, control);
        this.instance = instance;
        
        mainTreeView.setStatusColorEncoding(TraleSLDStepType.FINISHED, new Color(102,51,153));
        mainTreeView.setStatusColorEncoding(TraleSLDStepType.BLOCKED, Color.BLACK);
        //TODO: build font color customization facilities into TreeView
        //mainTreeView.setStatusFontColorEncoding(TraleSLDStepType.BLOCKED, Color.BLACK);
        
        mainChartView = new KahinaChartView();
        mainChartView.setTitle("Chart");
        control.registerListener("update", mainChartView);
        views.add(mainChartView);
        livingViews.add(mainChartView);
    }
    
    @Override
    protected void displayMainViews()
    {
    	super.displayMainViews();
        mainChartView.display(instance.getState().getChart()); 
    	
    }
}
