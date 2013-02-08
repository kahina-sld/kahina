package org.kahina.logic.sat.freemuc.gui;

import java.awt.Color;
import java.util.HashMap;

import org.kahina.core.KahinaStep;
import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.dag.ColoredPathDAGView;
import org.kahina.core.visual.dag.LayeredLayouter;
import org.kahina.core.visual.tree.KahinaTreeViewOptions;
import org.kahina.logic.sat.data.free.BooleanFormula;
import org.kahina.logic.sat.freemuc.FreeMUCInstance;
import org.kahina.logic.sat.freemuc.FreeMUCState;
import org.kahina.logic.sat.freemuc.FreeMUCStep;
import org.kahina.logic.sat.freemuc.visual.SATCheckingFormulaTreeView;
import org.kahina.logic.sat.muc.MUCStepType;
import org.kahina.logic.sat.visual.free.FormulaTreeView;

public class FreeMUCGUI extends KahinaGUI
{
    protected FormulaTreeView formulaView;
    protected ColoredPathDAGView decisionGraphView;
    
    private FreeMUCInstance kahina;
    
    public FreeMUCGUI(Class<? extends KahinaStep> stepType, FreeMUCInstance kahina, KahinaController control)
    {
        super(stepType, kahina); 
        this.kahina = kahina;
        initialize();
    }
    
    public void initialize()
    {
        super.initialize();
        
        //deactivate code location and message console views!
        varNameToView.remove("codeLocation");
        varNameToView.remove("messageConsole");
        
        formulaView = new SATCheckingFormulaTreeView(kahina);
        formulaView.setTitle("Formula Tree");
        
        formulaView.getConfig().setNodeSize(8);
        formulaView.getConfig().setHorizontalDistance(12);
        formulaView.getConfig().setVerticalDistance(10);
        formulaView.getConfig().setLineShapePolicy(KahinaTreeViewOptions.STRAIGHT_LINES);
        formulaView.getConfig().setNodePositionPolicy(KahinaTreeViewOptions.CENTERED_NODES);
        formulaView.getConfig().setEdgeTagPolicy(KahinaTreeViewOptions.NO_EDGE_TAGS);
        formulaView.setStatusColorEncoding(0, Color.YELLOW);
        formulaView.setStatusColorEncoding(1, Color.WHITE);
        formulaView.setStatusColorEncoding(2, Color.RED);
        formulaView.setStatusColorEncoding(3, Color.GREEN);
        formulaView.setStatusColorEncoding(4, new Color(102, 153, 102));
        
        kahina.registerSessionListener(KahinaEventTypes.UPDATE, formulaView);
        kahina.registerSessionListener("clauseSelection", this);
        views.add(formulaView);
        livingViews.add(formulaView);
        varNameToView.put("formulaTree", formulaView);
        
        decisionGraphView = new ColoredPathDAGView(kahina, new LayeredLayouter());
        decisionGraphView.setTitle("Decision graph");
        kahina.registerSessionListener(KahinaEventTypes.UPDATE, decisionGraphView);
        views.add(decisionGraphView);
        livingViews.add(decisionGraphView);
        varNameToView.put("decisionGraph", decisionGraphView);
        
        Color NICE_GREEN = new Color(102, 153, 102);
        Color NICE_RED = new Color(183, 50, 50);
        
        decisionGraphView.setStatusColorEncoding(MUCStepType.UNKNOWN, Color.WHITE);
        decisionGraphView.setStatusColorEncoding(MUCStepType.REDUCIBLE, NICE_GREEN);
        decisionGraphView.setStatusColorEncoding(MUCStepType.MINIMAL, NICE_RED);
    }
    
    public void displayMainViews()
    {
        FreeMUCState state = (FreeMUCState) kahina.getState();
        //decisionGraphView.display(state.getDecisionGraph());
        BooleanFormula frm = state.getFormula();
        if (frm != null)
        {
            formulaView.displayFormula(frm);
        }
        else
        {
            formulaView.displayText("No formula loaded yet.");
        }
    }
    
    @Override
    protected KahinaWindowManager createWindowManager()
    {
        return new FreeMUCWindowManager(kahina);
    }
    
    @Override
    /*public void prepare()
    {
        try
        {
            //this was only necessary for generating a default perspective
            HashMap<String, KahinaView<? extends KahinaObject>> nameToView;
            nameToView = new HashMap<String, KahinaView<? extends KahinaObject>>();
            nameToView.put("decisionGraph", decisionGraphView);
            nameToView.put("formulaTree", formulaView);
            KahinaPerspective psp = KahinaPerspective.generateDefaultPerspective(nameToView);
            windowManager.setPerspective(psp);
            windowManager.createWindows();  
            displayMainViews();
            
            //TODO: load last perspective instead of only default perspective from XML
            //InputStream xmlStream = new BufferedInputStream(MUCGUI.class.getResourceAsStream("perspective-integrated.xml"));
            //KahinaPerspective psp = KahinaPerspective.importXML(XMLUtil.parseXMLStream(xmlStream, false).getDocumentElement());

        }
        catch (NullPointerException e)
        {
            e.printStackTrace();
        }
    }*/
    
    public void displayStepContent(int stepID)
    {
        //System.err.println("displayStepContent(" + stepID + ")");
        super.displayStepContent(stepID);
        FreeMUCStep step = (FreeMUCStep) kahina.getState().getSteps().retrieve(stepID);
        //formulaView.flushRedrawAgenda();
    }
}
