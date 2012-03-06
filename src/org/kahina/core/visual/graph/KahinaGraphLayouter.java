package org.kahina.core.visual.graph;

import java.util.Map;

public abstract class KahinaGraphLayouter
{
    //references to the graph view
    KahinaGraphView view;
    //output (always accessible as references from the view)
    protected Map<Integer,Integer> xCoord;
    protected Map<Integer,Integer> yCoord;
    
    public void newGraph(KahinaGraphView view)
    {
        this.view = view;
        xCoord = view.getXCoordinates();
        yCoord = view.getYCoordinates();
        
        computeInitialLayout();
    }
    
    public abstract void refreshCoordinates();
    
    public abstract int getDisplayHeight();
    public abstract int getDisplayWidth();
    
    public abstract void computeInitialLayout();
    
    public abstract void optimize();
    public abstract void optimizeVtxPosAllEdges(int v);
    public abstract void optimizeVtxPosVisibleEdges(int v);
}
