package org.kahina.core.visual.graph;

import java.awt.Color;

import org.kahina.core.visual.KahinaViewConfiguration;

public class KahinaGraphViewConfiguration extends KahinaViewConfiguration
{
    private int zoomLevel = 10;
    private Color bgColor = Color.WHITE;
    private int vertexShapePolicy = KahinaGraphViewOptions.POINT_VERTICES;
    private int edgeLabelPolicy = KahinaGraphViewOptions.NO_EDGE_LABELS;
    private int antialiasingPolicy = KahinaGraphViewOptions.NO_ANTIALIASING;
    
    public void zoomIn()
    {
        if (zoomLevel < 30)
        {
            zoomLevel += 1;
        } 
        else
        {
            System.err.println("No zoom levels beyond 30 allowed!");
        }
    }

    public void zoomOut()
    {
        if (zoomLevel > 6)
        {
            zoomLevel -= 1;
        } 
        else
        {
            System.err.println("No zoom levels below 6 allowed!");
        }
    }

    public void setZoomLevel(int level)
    {
        zoomLevel = level;
    }

    public int getZoomLevel()
    {
        return zoomLevel;
    }
    
    public void setBackgroundColor(Color bgColor)
    {
        if (bgColor == null)
        {
            System.err.println("WARNING: GraphView received null as background color! Defaulting to Color.WHITE!");
            this.bgColor = Color.WHITE;
        }
        else
        {
            this.bgColor = bgColor;
        }
    }
    
    public Color getBackgroundColor()
    {
        return bgColor;
    }
    
    public int getVertexShapePolicy()
    {
        return vertexShapePolicy;
    }

    public void setVertexShapePolicy(int newPolicy)
    {
        if (newPolicy >= 0 && newPolicy <= 2)
        {
            vertexShapePolicy = newPolicy;
        } 
        else
        {
            System.err.println("WARNING: unknown vertex shape policy value " + newPolicy);
        }
    }
    
    public int getEdgeLabelPolicy()
    {
        return edgeLabelPolicy;
    }

    public void setEdgeLabelPolicy(int edgeLabelPolicy)
    {
        if (edgeLabelPolicy >= 0 && edgeLabelPolicy <= 3)
        {
            this.edgeLabelPolicy = edgeLabelPolicy;
        } 
        else
        {
            System.err.println("WARNING: unknown edge label policy value " + edgeLabelPolicy);
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
        } else
        {
            System.err.println("WARNING: unknown antialiasing policy value " + newPolicy);
        }
    }
}
