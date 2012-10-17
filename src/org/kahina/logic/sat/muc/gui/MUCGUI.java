package org.kahina.logic.sat.muc.gui;

import java.awt.Color;
import java.io.BufferedInputStream;
import java.io.InputStream;

import org.kahina.core.KahinaStep;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.io.util.XMLUtil;
import org.kahina.core.visual.dag.ColoredPathDAGView;
import org.kahina.core.visual.dag.LayeredLayouter;
import org.kahina.core.visual.graph.KahinaGraphViewOptions;
import org.kahina.core.visual.graph.SpringLayouter;
import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.data.cnf.GroupCnfSatInstance;
import org.kahina.logic.sat.io.minisat.MiniSATFiles;
import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.muc.MUCState;
import org.kahina.logic.sat.muc.MUCStep;
import org.kahina.logic.sat.muc.MUCStepType;
import org.kahina.logic.sat.muc.heuristics.AlwaysFirstHeuristics;
import org.kahina.logic.sat.muc.heuristics.AlwaysLastHeuristics;
import org.kahina.logic.sat.muc.heuristics.CenterHeuristics;
import org.kahina.logic.sat.muc.visual.MUCStepController;
import org.kahina.logic.sat.muc.visual.UCReducerListView;
import org.kahina.logic.sat.visual.cnf.graph.KahinaGroupSatInstanceGraphView;
import org.kahina.logic.sat.visual.cnf.graph.KahinaSatInstanceGraphView;

public class MUCGUI extends KahinaGUI
{
    protected KahinaSatInstanceGraphView satInstanceView;
    protected ColoredPathDAGView decisionGraphView;
    protected MUCStepController stepController;
    protected UCReducerListView reducerListView;
    
    private MUCInstance kahina;
    
    public MUCGUI(Class<? extends KahinaStep> stepType, MUCInstance kahina)
    {
        super(stepType, kahina);
        this.kahina = kahina;   
        initialize();
    }
    
    public void initialize()
    {
        super.initialize();
        varNameToView.remove("controlFlowTree");
        
        satInstanceView = new KahinaGroupSatInstanceGraphView(kahina, new SpringLayouter());
        if (isInGroupMode())
        {
            satInstanceView.setTitle("Group SAT Instance");
        }
        else
        {
            satInstanceView.setTitle("SAT Instance");
        }
        kahina.registerInstanceListener(KahinaEventTypes.UPDATE, satInstanceView);
        kahina.registerInstanceListener("clauseSelection", this);
        views.add(satInstanceView);
        livingViews.add(satInstanceView);
        varNameToView.put("satInstance", satInstanceView);
        
        Color NICE_GREEN = new Color(102, 153, 102);
        Color NICE_RED = new Color(183, 50, 50);
        
        satInstanceView.setVertexStatusVertexColorEncoding(0, Color.WHITE);
        satInstanceView.setVertexStatusVertexColorEncoding(1, NICE_GREEN);
        satInstanceView.setVertexStatusVertexColorEncoding(2, NICE_RED);
        satInstanceView.setVertexStatusEdgeColorEncoding(0, 0, Color.BLACK);
        satInstanceView.setVertexStatusEdgeColorEncoding(0, 1, NICE_GREEN);
        satInstanceView.setVertexStatusEdgeColorEncoding(1, 0, NICE_GREEN);
        satInstanceView.setVertexStatusEdgeColorEncoding(1, 1, NICE_GREEN);
        satInstanceView.setVertexStatusEdgeColorEncoding(1, 2, NICE_GREEN);
        satInstanceView.setVertexStatusEdgeColorEncoding(2, 1, NICE_GREEN);
        satInstanceView.setVertexStatusEdgeColorEncoding(0, 2, Color.BLACK);
        satInstanceView.setVertexStatusEdgeColorEncoding(2, 0, Color.BLACK);
        satInstanceView.setVertexStatusEdgeColorEncoding(2, 2, NICE_RED);
        satInstanceView.getConfig().setVertexVisibilityPolicy(KahinaGraphViewOptions.VERTICES_SPECIAL_VISIBLE);
        satInstanceView.getConfig().setZoomLevel(5);
        satInstanceView.getConfig().setNodeSize(2);
        
        decisionGraphView = new ColoredPathDAGView(kahina, new LayeredLayouter());
        decisionGraphView.setTitle("Decision graph");
        kahina.registerInstanceListener(KahinaEventTypes.UPDATE, decisionGraphView);
        views.add(decisionGraphView);
        livingViews.add(decisionGraphView);
        varNameToView.put("decisionGraph", decisionGraphView);
        
        decisionGraphView.setStatusColorEncoding(MUCStepType.ACTIVE, Color.WHITE);
        decisionGraphView.setStatusColorEncoding(MUCStepType.MINIMAL, NICE_GREEN);
        decisionGraphView.setStatusColorEncoding(MUCStepType.SAT, NICE_RED);
        
        stepController = new MUCStepController(kahina);
        kahina.registerInstanceListener(KahinaEventTypes.SELECTION, stepController);
        views.add(stepController);
        livingViews.add(stepController);
        varNameToView.put("stepController", stepController);
        
        //TODO: hand over a useful files object
        reducerListView = new UCReducerListView(kahina, new MiniSATFiles());
        reducerListView.addHeuristic(AlwaysFirstHeuristics.class);
        reducerListView.addHeuristic(CenterHeuristics.class);
        reducerListView.addHeuristic(AlwaysLastHeuristics.class);
        views.add(reducerListView);
        livingViews.add(reducerListView);
        varNameToView.put("reducers", reducerListView);
        
        decisionGraphView.setStatusColorEncoding(MUCStepType.ACTIVE, Color.WHITE);
        decisionGraphView.setStatusColorEncoding(MUCStepType.MINIMAL, new Color(102, 153, 102));
        decisionGraphView.setStatusColorEncoding(MUCStepType.SAT, new Color(183, 50, 50));
        //decisionGraphView.getConfig().setLineShapePolicy(KahinaTreeViewOptions.STRAIGHT_LINES);
        //decisionGraphView.getConfig().setAutoscrollPolicy(KahinaTreeViewOptions.NO_AUTOSCROLL);
        //decisionGraphView.getConfig().setEdgeTagPolicy(KahinaTreeViewOptions.NO_EDGE_TAGS);
        //decisionGraphView.getConfig().setCollapsePolicy(KahinaTreeViewOptions.NO_COLLAPSING);
        decisionGraphView.getConfig().setVerticalDistance(5);
        decisionGraphView.getConfig().setHorizontalDistance(3);
    }
    
    public void displayMainViews()
    {
        MUCState state = (MUCState) kahina.getState();
        decisionGraphView.display(state.getDecisionGraph());
        reducerListView.display(state.getReducers());
        CnfSatInstance sat = state.getSatInstance();
        if (sat != null)
        {
            if (isInGroupMode())
            {
                ((KahinaGroupSatInstanceGraphView) satInstanceView).display((GroupCnfSatInstance) sat);
            }
            else
            {
                satInstanceView.display(sat);
            }
        }
        else
        {
            satInstanceView.displayText("No SAT Instance loaded yet.");
        }
    }
    
    @Override
    protected KahinaWindowManager createWindowManager()
    {
        System.err.println("MUCGUI.createWindowManager()");
        return new MUCWindowManager(kahina);
    }
    
    public KahinaPerspective generateInitialPerspective()
    {
        //TODO: load last perspective instead of only default perspective from XML
        InputStream xmlStream = new BufferedInputStream(MUCGUI.class.getResourceAsStream("perspective-integrated.xml"));
        return KahinaPerspective.importXML(XMLUtil.parseXMLStream(xmlStream, false).getDocumentElement());
    }
    
    public void displayStepContent(int stepID)
    {
        System.err.println("displayStepContent(" + stepID + ")");
        super.displayStepContent(stepID);
        MUCStep step = (MUCStep) kahina.getState().getSteps().retrieve(stepID);
        if (satInstanceView.showsClauseGraph())
        {
            satInstanceView.setSpecialVertices(step.getUc());
            for (int ic : step.getUc())
            {
                satInstanceView.getModel().setVertexStatus(ic, step.getIcStatus(ic));
            }
        }
        else
        {
            satInstanceView.turnSpecialVerticesBackToNormal();
            //TODO: show only edges corresponding to clauses in UC!
        }
        satInstanceView.flushRedrawAgenda();
    }
    
    //in this somewhat roundabout way, handing on the mode information is avoided
    private boolean isInGroupMode()
    {
        return ((MUCState) kahina.getState()).getSatInstance() instanceof GroupCnfSatInstance;
    }
    
    public void processEvent(KahinaEvent e)
    {
        if (e.getType().equals("clauseSelection"))
        {
            processEvent((ClauseSelectionEvent) e);
        }
        else
        {
            super.processEvent(e);
        }
    }
    
    public void processEvent(ClauseSelectionEvent e)
    {
        if (satInstanceView.showsClauseGraph())
        {
            satInstanceView.getRedrawAgenda().add(satInstanceView.getMarkedVertex());
            satInstanceView.setMarkedVertex(e.getClauseID());
            satInstanceView.getRedrawAgenda().add(e.getClauseID());
            kahina.dispatchInstanceEvent(new KahinaRedrawEvent());
        }
    }
}
