package org.kahina.core.visual.chart;

import java.awt.Color;

import org.kahina.core.visual.KahinaViewConfiguration;

public class KahinaChartViewConfiguration extends KahinaViewConfiguration
{
    //display options
    private int cellWidth = 150; 
    Color bgColor = Color.WHITE;
    int cellWidthPolicy = KahinaChartViewOptions.MINIMAL_NECESSARY_WIDTH;
    int edgeStackingPolicy = KahinaChartViewOptions.STACK_EDGES_BY_ID;
    int displayOrientation = KahinaChartViewOptions.BOTTOM_UP_DISPLAY;
    int displayRangePolicy = KahinaChartViewOptions.RANGE_USED_OR_CAPTION_DEFINED;
    int dependencyDisplayPolicy = KahinaChartViewOptions.BOTH_ANCESTORS_AND_DESCENDANTS;
    int antialiasingPolicy = KahinaChartViewOptions.ANTIALIASING;
    boolean transitiveAncestors;
    boolean transitiveDescendants;
    KahinaChartEdgeDisplayDecider displayDecider;
    int fontSize; //also determines zoom factor and cell height
    
    public KahinaChartViewConfiguration()
    {
        transitiveAncestors = false;
        transitiveDescendants = false;
        fontSize = 10;
    }
    
    public void zoomIn()
    {
        if (fontSize < 20)
        {
            fontSize += 1;
        }
        else
        {
            System.err.println("No zoom levels beyond 20 allowed!");
        }
    }
    
    public void zoomOut()
    {
        if (fontSize > 4)
        {
            fontSize -= 1;
        }
        else
        {
            System.err.println("No zoom levels below 4 allowed!");
        }
    }
    
    public void setZoomLevel(int level)
    {
        fontSize = level;
    }
    
    public int getZoomLevel()
    {
        return fontSize;
    }
    
    public void setCellWidthPolicy(int newPolicy)
    {
        if (newPolicy >= 0 && newPolicy <= 2)
        {
            cellWidthPolicy = newPolicy;
        }
        else
        {
            System.err.println("WARNING: unknown cell width policy value " + newPolicy);
        }
    }
    
    public void setDisplayOrientation(int newPolicy)
    {
        if (newPolicy >= 0 && newPolicy <= 1)
        {
            displayOrientation = newPolicy;
        }
        else
        {
            System.err.println("WARNING: unknown displayOrientation value " + newPolicy);
        }
    }
    
    public void setDisplayRangePolicy(int newPolicy)
    {
        if (newPolicy >= 0 && newPolicy <= 2)
        {
            displayRangePolicy = newPolicy;
        }
        else
        {
            System.err.println("WARNING: unknown display range policy value " + newPolicy);
        }
    }
    
    public void setEdgeStackingPolicy(int newPolicy)
    {
        if (newPolicy >= 0 && newPolicy <= 1)
        {
            edgeStackingPolicy = newPolicy;
        }
        else
        {
            System.err.println("WARNING: unknown edge stacking policy value " + newPolicy);
        }
    }
    
    public int getCellWidthPolicy()
    {
        return cellWidthPolicy;
    }
    
    public int getEdgeStackingPolicy()
    {
        return edgeStackingPolicy;
    }
    
    public int getDisplayOrientation()
    {
        return displayOrientation;
    }
    
    public int getDisplayRangePolicy()
    {
        return displayRangePolicy;
    }
    
    public int getDependencyDisplayPolicy()
    {
        return dependencyDisplayPolicy;
    }
    
    public void setDependencyDisplayPolicy(int newPolicy)
    {
        if (newPolicy >= 0 && newPolicy <= 3)
        {
            dependencyDisplayPolicy = newPolicy;
        }
        else
        {
            System.err.println("WARNING: unknown dependency display policy value " + newPolicy);
        }
    }
    
    public int getAntialiasingPolicy()
    {
        return antialiasingPolicy;
    }

    public void setAntialiasingPolicy(int newPolicy)
    {
        if (newPolicy >= 0 && newPolicy <= 1)
        {
            antialiasingPolicy = newPolicy;
        }
        else
        {
            System.err.println("WARNING: unknown antialiasing policy value " + newPolicy);
        }
    }
    
    public boolean getAncestorTransitivity()
    {
        return transitiveAncestors;
    }
    
    public void swapAncestorTransitivity()
    {
        transitiveAncestors = !transitiveAncestors;
    }
    
    public boolean getDescendantTransitivity()
    {
        return transitiveDescendants;
    }
    
    public void swapDescendantTransitivity()
    {
        transitiveDescendants = !transitiveDescendants;
    }
    
    public boolean decideEdgeDisplay(int edgeID)
    {
        if (displayDecider == null) return true;
        return displayDecider.decideEdgeDisplay(edgeID);
    }
    
    public void setCellWidth(int cellWidth)
    {
        this.cellWidth = cellWidth;
    }

    public int getCellWidth()
    {
        return cellWidth;
    }
}
