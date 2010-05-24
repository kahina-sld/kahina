package org.kahina.tralesld.gui;

import java.awt.Color;

import org.kahina.core.KahinaStep;
import org.kahina.core.gui.KahinaViewIntegrationType;
import org.kahina.core.visual.chart.KahinaChartView;
import org.kahina.lp.gui.LogicProgrammingGUI;
import org.kahina.tralesld.TraleSLDInstance;
import org.kahina.tralesld.TraleSLDStepType;
import org.kahina.tralesld.data.chart.TraleSLDChartEdgeStatus;
import org.kahina.tralesld.data.tree.TraleSLDLayerDecider;
import org.kahina.tralesld.visual.chart.TraleSLDChartEdgeDisplayDecider;

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
        views.add(mainChartView);
        livingViews.add(mainChartView); 
        varNameToView.put("chart", mainChartView);

        mainTreeView.setStatusColorEncoding(TraleSLDStepType.FINISHED, new Color(102,51,153));
        mainTreeView.setStatusColorEncoding(TraleSLDStepType.BLOCKED, Color.BLACK);
        //TODO: build font color customization facilities into TreeView
        //mainTreeView.setStatusFontColorEncoding(TraleSLDStepType.BLOCKED, Color.BLACK);
        
        mainChartView.setStatusColorEncoding(TraleSLDChartEdgeStatus.PROSPECTIVE, Color.LIGHT_GRAY);
        mainChartView.setStatusColorEncoding(TraleSLDChartEdgeStatus.SUCCESSFUL, new Color(102,153,102));
        mainChartView.setStatusColorEncoding(TraleSLDChartEdgeStatus.FAILED, new Color(183,50,50));
        mainChartView.setStatusHighlightColorEncoding(TraleSLDChartEdgeStatus.PROSPECTIVE, Color.WHITE);
        mainChartView.setStatusHighlightColorEncoding(TraleSLDChartEdgeStatus.SUCCESSFUL, Color.GREEN);
        mainChartView.setStatusHighlightColorEncoding(TraleSLDChartEdgeStatus.FAILED, Color.RED);
        mainChartView.setDisplayDecider(new TraleSLDChartEdgeDisplayDecider());
    }
    
    @Override
    protected void displayMainViews()
    {
    	super.displayMainViews();
        //set deciders here because the trees are generated generically by the KahinaState
        mainTreeView.getModel().setLayerDecider(new TraleSLDLayerDecider(2));
        mainTreeView.getSecondaryModel().setLayerDecider(new TraleSLDLayerDecider(2));
        mainChartView.display(instance.getState().getChart());  
    }
    
    public void prepare()
    {
        super.prepare();
        integrateVariableDisplays(KahinaViewIntegrationType.VERTICAL, "startBindings", "endBindings", "Variable bindings");
        integrateVariableDisplays(KahinaViewIntegrationType.VERTICAL, "codeLocation", "messageConsole", "Source & Console");
        integrateVariableDisplays(KahinaViewIntegrationType.HORIZONTAL, "startFeatStruct", "endFeatStruct", "Feature Structures");
        //slightly hacky: allow direct manipulation of view components
        getWindowForVarName("codeLocation").setSize(400, 500);
        getWindowForVarName("codeLocation").setLocation(400, 0);
        getWindowForVarName("startFeatStruct").setSize(700, 300);
        getWindowForVarName("startFeatStruct").setLocation(0, 550);
        getWindowForVarName("startBindings").setSize(200, 300);
        getWindowForVarName("startBindings").setLocation(700, 550);
        getWindowForVarName("controlFlowTree").setSize(500, 800);
        getWindowForVarName("controlFlowTree").setLocation(800, 0);
        getWindowForVarName("chart").setSize(400, 400);
        getWindowForVarName("chart").setLocation(0, 150);
    }
}
