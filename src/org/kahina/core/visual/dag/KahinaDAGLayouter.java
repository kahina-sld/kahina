package org.kahina.core.visual.dag;

import java.util.Map;

public abstract class KahinaDAGLayouter
{
    //references to the graph view
    KahinaDAGView view;
    //output (always accessible as references from the view)
    protected Map<Integer,Integer> xCoord;
    protected Map<Integer,Integer> yCoord;
    
    public void newDAG(KahinaDAGView view)
    {
        this.view = view;
        xCoord = view.getXCoordinates();
        yCoord = view.getYCoordinates();
        
        computeLayout();
    }
    
    public abstract void refreshCoordinates();
    
    public abstract int getDisplayHeight();
    public abstract int getDisplayWidth();
    
    public abstract void computeLayout();
    
    public abstract int getNodeAtCoordinates(int x, int y);
}
