package org.kahina.logic.sat.insertionmus.gui;

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
import org.kahina.core.visual.tree.KahinaTreeViewOptions;
import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.data.cnf.GroupCnfSatInstance;
import org.kahina.logic.sat.io.minisat.MiniSATFiles;
import org.kahina.logic.sat.insertionmus.MUCInstance;
import org.kahina.logic.sat.insertionmus.MUCState;
import org.kahina.logic.sat.insertionmus.MUCStep;
//import org.kahina.logic.sat.muc.MUCStepType;
import org.kahina.logic.sat.muc.MUCStepType;
import org.kahina.logic.sat.muc.MetaLearningMode;
import org.kahina.logic.sat.insertionmus.gui.MUCWindowManager;
import org.kahina.logic.sat.muc.heuristics.AscendingIndexHeuristic;
import org.kahina.logic.sat.muc.heuristics.AscendingRelevanceHeuristic;
import org.kahina.logic.sat.muc.heuristics.CenteredRelevanceHeuristic;
import org.kahina.logic.sat.muc.heuristics.DescendingIndexHeuristic;
import org.kahina.logic.sat.muc.heuristics.CenteredIndexHeuristic;
import org.kahina.logic.sat.muc.heuristics.DescendingRelevanceHeuristic;
import org.kahina.logic.sat.muc.visual.MUCStepController;
import org.kahina.logic.sat.insertionmus.visual.MUCStepViewContaining;
import org.kahina.logic.sat.insertionmus.visual.MUCStepViewRemaining;
import org.kahina.logic.sat.insertionmus.visual.UCReducerListViewPanel;
import org.kahina.logic.sat.muc.visual.MetaInstanceView;
import org.kahina.logic.sat.muc.visual.PartitionBlockView;
import org.kahina.logic.sat.muc.visual.RecursiveBlockView;
import org.kahina.logic.sat.muc.visual.UCReducerListView;
import org.kahina.logic.sat.visual.cnf.graph.KahinaGroupSatInstanceGraphView;
import org.kahina.logic.sat.visual.cnf.graph.KahinaSatInstanceGraphView;
import org.kahina.logic.sat.visual.cnf.list.KahinaSatInstanceListView;
import org.kahina.qtype.gui.QTypeGUI;

public class MUCGUI extends KahinaGUI
{
    private static final boolean VERBOSE = false;
    
//    protected KahinaSatInstanceListView satInstanceView;
//    protected KahinaSatInstanceListView metaInstanceView;
    protected ColoredPathDAGView decisionGraphView;
    //protected MUCStepController stepController;
//    protected PartitionBlockView blockListView;
//    protected RecursiveBlockView blockTreeView;
    protected MUCStepViewRemaining mucViewRemaining;
    protected MUCStepViewContaining mucViewContaining;
    protected UCReducerListViewPanel reducerListView;
    
    private MUCInstance kahina;
    
    public MUCGUI(Class<? extends KahinaStep> stepType, MUCInstance kahina)
    {
        super(stepType, kahina);
        this.kahina = kahina;   
        initialize();
    }
//    public MUCGUI(Class<? extends KahinaStep> stepType, MUCInstance kahina)
//    {
//        super(stepType, kahina);
//        this.kahina = kahina;   
//        initialize();
//    }
    
    public void initialize()
    {
        super.initialize();
        varNameToView.remove("controlFlowTree");
        
        kahina.registerInstanceListener("clauseSelection", this);
        
//        satInstanceView = new KahinaSatInstanceListView(kahina);
//        if (isInGroupMode())
//        {
//            satInstanceView.setTitle("Group SAT Instance");
//        }
//        else
////        {
//            satInstanceView.setTitle("SAT Instance");
////        }
//        kahina.registerInstanceListener(KahinaEventTypes.UPDATE, satInstanceView);
//        views.add(satInstanceView);
//        livingViews.add(satInstanceView);
//        varNameToView.put("satInstance", satInstanceView);
        
        
        
        Color NICE_GREEN = new Color(102, 153, 102);
        Color NICE_RED = new Color(183, 50, 50);
        Color LIGHT_RED = new Color(203, 153, 153);
        Color LIGHT_GREEN = new Color(152, 203, 102);
        
        /*satInstanceView.setVertexStatusVertexColorEncoding(0, Color.WHITE);
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
        satInstanceView.getConfig().setNodeSize(2);*/
        
        decisionGraphView = new ColoredPathDAGView(kahina, new LayeredLayouter());
        decisionGraphView.setTitle("Decision graph");
        kahina.registerInstanceListener(KahinaEventTypes.UPDATE, decisionGraphView);
        views.add(decisionGraphView);
        livingViews.add(decisionGraphView);
        varNameToView.put("decisionGraph", decisionGraphView);
        
        decisionGraphView.setStatusColorEncoding(MUCStepType.UNKNOWN, Color.WHITE);
        decisionGraphView.setStatusColorEncoding(MUCStepType.COMPLETE, NICE_GREEN);
        decisionGraphView.setStatusColorEncoding(MUCStepType.MINIMAL, NICE_RED);
        decisionGraphView.setStatusColorEncoding(MUCStepType.REDUCIBLE, LIGHT_GREEN);
        
        //this one is not needed any longer, it became obsolete with the mucView
        /*stepController = new MUCStepController(kahina);
        kahina.registerInstanceListener(KahinaEventTypes.SELECTION, stepController);
        views.add(stepController);
        livingViews.add(stepController);
        varNameToView.put("stepController", stepController);*/
        
       
        
        mucViewRemaining = new MUCStepViewRemaining(kahina);
        kahina.registerInstanceListener(KahinaEventTypes.SELECTION, mucViewRemaining);
        views.add(mucViewRemaining);
        livingViews.add(mucViewRemaining);
        varNameToView.put("remainingUC", mucViewRemaining);
        

        mucViewContaining = new MUCStepViewContaining(kahina);
        kahina.registerInstanceListener(KahinaEventTypes.SELECTION, mucViewContaining);
        views.add(mucViewContaining);
        livingViews.add(mucViewContaining);
        varNameToView.put("currentUC", mucViewContaining);
        
//        mucView.setStatusColorEncoding(0, Color.BLACK);
//        mucView.setStatusColorEncoding(1, NICE_GREEN);
//        mucView.setStatusColorEncoding(2, NICE_RED);
//        mucView.setStatusColorEncoding(3, LIGHT_GREEN);
        
        //TODO: hand over a useful files object
        reducerListView = new UCReducerListViewPanel(kahina);
//        reducerListView.addHeuristic(AscendingIndexHeuristic.class);
//        reducerListView.addHeuristic(CenteredIndexHeuristic.class);
//        reducerListView.addHeuristic(DescendingIndexHeuristic.class);
//        reducerListView.addHeuristic(AscendingRelevanceHeuristic.class);
//        reducerListView.addHeuristic(CenteredRelevanceHeuristic.class);
//        reducerListView.addHeuristic(DescendingRelevanceHeuristic.class);
        views.add(reducerListView);
        livingViews.add(reducerListView);
        varNameToView.put("reducers", reducerListView);
        
        //decisionGraphView.getConfig().setLineShapePolicy(KahinaTreeViewOptions.STRAIGHT_LINES);
        //decisionGraphView.getConfig().setAutoscrollPolicy(KahinaTreeViewOptions.NO_AUTOSCROLL);
        //decisionGraphView.getConfig().setEdgeTagPolicy(KahinaTreeViewOptions.NO_EDGE_TAGS);
        //decisionGraphView.getConfig().setCollapsePolicy(KahinaTreeViewOptions.NO_COLLAPSING);
        decisionGraphView.getConfig().setVerticalDistance(5);
        decisionGraphView.getConfig().setHorizontalDistance(8);
    }
    
    public void displayMainViews()
    {
        if (VERBOSE) System.err.println("MUCGUI.displayMainViews()");
        MUCState state = (MUCState) kahina.getState();
        decisionGraphView.display(state.getDecisionGraph());
//        reducerListView.display(state.getReducers());
        CnfSatInstance sat = state.getSatInstance();
        System.out.println(sat);
//        if (VERBOSE) System.err.println("  state.getSatInstance() = " + sat);
        if (sat != null)
        {
//            if (isInGroupMode())
//            {
//                //TODO: treat group SAT instances in list view as well!
//                //((KahinaGroupSatInstanceGraphView) satInstanceView).display((GroupCnfSatInstance) sat);
//            }
//            else
//            {
//                if (VERBOSE) System.err.println("    satInstanceView.display(" + sat + ")");
//                satInstanceView.display(sat);
//            }
//                  
//            if (metaInstanceView != null) 
//            {
//                if (VERBOSE) System.err.println("    metaInstanceView.display(" + state.getMetaInstance() + ")");
//                metaInstanceView.display(state.getMetaInstance());
//            }
//            if (blockListView != null)
//            {
//                if (VERBOSE) System.err.println("    blockView.display(" + state.getPartitionBlocks() + ")");
//                blockListView.display(state.getPartitionBlocks());
//            }
//            if (blockTreeView != null)
//            {
//                if (VERBOSE) System.err.println("    blockView.display(" + state.getPartitionBlocks() + ")");
//                blockTreeView.display(state.getRecursiveBlocks());
//            }
//            if (VERBOSE) System.err.println("    mucView.display(" + sat + ")");
            mucViewRemaining.display(sat);
            mucViewContaining.display(sat);
        }
//        else
//        {
//            if (VERBOSE) System.err.println("    displaying \"No instance loaded\" messages");
//            satInstanceView.displayText("No SAT Instance loaded yet.");
//            if (metaInstanceView != null) metaInstanceView.displayText("No SAT Instance loaded yet.");
////            mucView.displayText("No SAT Instance loaded yet.");
//            if (blockListView != null) blockListView.displayText("No SAT Instance loaded yet.");
//            //if (blockTreeView != null) blockTreeView.displayText("No SAT Instance loaded yet.");
//        }
    }
    
    @Override
    protected KahinaWindowManager createWindowManager()
    {
        return new MUCWindowManager(kahina);
    }
    
    public KahinaPerspective generateInitialPerspective()
    {
        //TODO: load last perspective instead of only default perspective from XML
        InputStream xmlStream = new BufferedInputStream(MUCGUI.class.getResourceAsStream("perspective-musticca-default.xml"));
        return KahinaPerspective.importXML(XMLUtil.parseXMLStream(xmlStream, false).getDocumentElement());
    }
    
    public void displayStepContent(int stepID)
    {
        //System.err.println("displayStepContent(" + stepID + ")");
        super.displayStepContent(stepID);
        MUCStep step = (MUCStep) kahina.getState().getSteps().retrieve(stepID);
        //TODO: mark the current MUS in the sat instance display
        
        /*if (satInstanceView.showsClauseGraph())
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
        satInstanceView.flushRedrawAgenda();*/
    }
    
    //in this somewhat roundabout way, handing on the mode information is avoided
//    private boolean isInGroupMode()
//    {
////        return ((MUCState) kahina.getState()).getSatInstance() instanceof GroupCnfSatInstance;
//    }
//    
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
        System.out.println("Gui " +e);
    }
    
    public void processEvent(ClauseSelectionEvent e)
    {
        /*if (satInstanceView.showsClauseGraph())
        {
            satInstanceView.getRedrawAgenda().add(satInstanceView.getMarkedVertex());
            satInstanceView.setMarkedVertex(e.getClauseID());
            satInstanceView.getRedrawAgenda().add(e.getClauseID());
            kahina.dispatchInstanceEvent(new KahinaRedrawEvent());
        }*/
    }
}
