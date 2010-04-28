package org.kahina.tralesld.gui;

import java.awt.Color;

import org.kahina.core.KahinaRunner;
import org.kahina.core.KahinaStep;
import org.kahina.core.visual.chart.KahinaChartView;
import org.kahina.lp.gui.LogicProgrammingGUI;
import org.kahina.tralesld.TraleSLDInstance;
import org.kahina.tralesld.TraleSLDStepType;
import org.kahina.tralesld.data.chart.TraleSLDChartEdgeStatus;
import org.kahina.tralesld.data.tree.TraleSLDLayerDecider;

public class TraleSLDGUI extends LogicProgrammingGUI
{
	private TraleSLDInstance instance;
	
	protected KahinaChartView mainChartView;
	
    public TraleSLDGUI(Class<? extends KahinaStep> stepType, TraleSLDInstance instance)
    {
        super(stepType, instance);
        this.instance = instance;      
        
        mainChartView = new KahinaChartView();
        mainChartView.setTitle("Chart");
        KahinaRunner.getControl().registerListener("update", mainChartView);
        views.add(mainChartView);
        livingViews.add(mainChartView);
        

        mainTreeView.setStatusColorEncoding(TraleSLDStepType.FINISHED, new Color(102,51,153));
        mainTreeView.setStatusColorEncoding(TraleSLDStepType.BLOCKED, Color.BLACK);
        //TODO: build font color customization facilities into TreeView
        //mainTreeView.setStatusFontColorEncoding(TraleSLDStepType.BLOCKED, Color.BLACK);
        
        mainChartView.setStatusColorEncoding(TraleSLDChartEdgeStatus.ACTIVE, Color.WHITE);
        mainChartView.setStatusColorEncoding(TraleSLDChartEdgeStatus.SUCCESSFUL, new Color(102,153,102));
        mainChartView.setStatusColorEncoding(TraleSLDChartEdgeStatus.FAILED, new Color(183,50,50));
    }
    
    @Override
    protected void displayMainViews()
    {
    	super.displayMainViews();
        //set deciders here because the trees are generated generically by the KahinaState
        mainTreeView.getModel().setLayerDecider(new TraleSLDLayerDecider());
        mainTreeView.getSecondaryModel().setLayerDecider(new TraleSLDLayerDecider());
        mainChartView.display(instance.getState().getChart());  	
    }
}
