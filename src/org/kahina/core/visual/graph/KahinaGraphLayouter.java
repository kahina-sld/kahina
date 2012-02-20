package org.kahina.core.visual.graph;

import java.util.Map;

import org.kahina.core.data.graph.KahinaGraph;

public abstract class KahinaGraphLayouter
{
    //references to the relevant parts of a KahinaGraphView
    //input
    protected KahinaGraph g;
    protected KahinaGraphViewConfiguration config;
    //output (always accessible as references from the view)
    protected Map<Integer,Integer> xCoord;
    protected Map<Integer,Integer> yCoord;
    
    public void newGraph(KahinaGraphView view)
    {
        g = view.getModel();
        config = view.getConfig();
        xCoord = view.getXCoordinates();
        yCoord = view.getYCoordinates();
        
        computeInitialLayout();
    }
    
    public abstract void computeInitialLayout();
    
    public abstract void optimize();
}
