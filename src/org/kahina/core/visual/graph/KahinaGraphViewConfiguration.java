package org.kahina.core.visual.graph;

import java.awt.Color;

import org.kahina.core.visual.KahinaViewConfiguration;

public class KahinaGraphViewConfiguration extends KahinaViewConfiguration
{
    private int zoomLevel = 10;
    private int nodeSize = 10;
    private Color bgColor = Color.WHITE;
    private int vertexShapePolicy = KahinaGraphViewOptions.POINT_VERTICES;
    private int edgeLabelPolicy = KahinaGraphViewOptions.NO_EDGE_LABELS;
    private int drawingOrderPolicy = KahinaGraphViewOptions.VERTICES_ABOVE_EDGES;
    private int antialiasingPolicy = KahinaGraphViewOptions.NO_ANTIALIASING;
    private int graphLayout = KahinaGraphViewOptions.LAYOUT_GRID;
    private int vertexVisibilityPolicy = KahinaGraphViewOptions.VERTICES_EXPLICITLY_VISIBLE;
    private int edgeVisibilityPolicy = KahinaGraphViewOptions.EDGES_WITH_BOTH_NODES_VISIBLE;
    private int edgeColoringPolicy = KahinaGraphViewOptions.EDGE_COLOR_FUNCTION_OF_VERTEX_COLOR;
    private int specialVertexPositionPolicy = KahinaGraphViewOptions.SPECIAL_VERTICES_MIXED;
    private int specialVertexColoringPolicy = KahinaGraphViewOptions.SPECIAL_VERTICES_NORMAL_COLOR;
    
    public void zoomIn()
    {
        if (zoomLevel < 50)
        {
            zoomLevel += 1;
        } 
        else
        {
            System.err.println("No zoom levels beyond 50 allowed!");
        }
    }

    public void zoomOut()
    {
        if (zoomLevel > 1)
        {
            zoomLevel -= 1;
        } 
        else
        {
            System.err.println("No zoom levels below 1 allowed!");
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
    
    public void increaseNodeSize()
    {
        if (zoomLevel < 20)
        {
            zoomLevel += 1;
        } 
        else
        {
            System.err.println("No node sizes beyond 20 allowed!");
        }
    }

    public void decreaseNodeSize()
    {
        if (nodeSize > 1)
        {
            nodeSize -= 1;
        } 
        else
        {
            System.err.println("No node sizes below 1 allowed!");
        }
    }
    
    public void setNodeSize(int size)
    {
        nodeSize = size;
    }

    public int getNodeSize()
    {
        return nodeSize;
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
    
    public int getDrawingOrderPolicy()
    {
        return drawingOrderPolicy;
    }

    public void setDrawingOrderPolicy(int newPolicy)
    {
        if (newPolicy >= 0 && newPolicy <= 1)
        {
            drawingOrderPolicy = newPolicy;
        } 
        else
        {
            System.err.println("WARNING: unknown drawing order policy value " + newPolicy);
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
    
    public int getGraphLayout()
    {
        return graphLayout;
    }

    public void setGraphLayout(int newLayout)
    {
        if (newLayout >= 0 && newLayout <= 2)
        {
            graphLayout = newLayout;
        } 
        else
        {
            System.err.println("WARNING: unknown graph layout value " + newLayout);
        }
    }

    public void setVertexVisibilityPolicy(int vertexVisibilityPolicy)
    {
        if (vertexVisibilityPolicy >= 0 && vertexVisibilityPolicy <= 2)
        {
            this.vertexVisibilityPolicy = vertexVisibilityPolicy;
        }
        else
        {
            System.err.println("WARNING: unknown vertex visibility policy value " + vertexVisibilityPolicy);
        }
    }

    public int getVertexVisibilityPolicy()
    {
        return vertexVisibilityPolicy;
    }

    public void setEdgeVisibilityPolicy(int edgeVisibilityPolicy)
    {
        if (edgeVisibilityPolicy >= 0 && edgeVisibilityPolicy <= 2)
        {
            this.edgeVisibilityPolicy = edgeVisibilityPolicy;
        }
        else
        {
            System.err.println("WARNING: unknown edge visibility policy value " + edgeVisibilityPolicy);
        }
    }

    public int getEdgeVisibilityPolicy()
    {
        return edgeVisibilityPolicy;
    }

    public void setEdgeColoringPolicy(int edgeColoringPolicy)
    {
        if (edgeColoringPolicy >= 0 && edgeColoringPolicy <= 2)
        {
            this.edgeColoringPolicy = edgeColoringPolicy;
        }
        else
        {
            System.err.println("WARNING: unknown edge coloring policy value " + edgeColoringPolicy);
        }
    }

    public int getEdgeColoringPolicy()
    {
        return edgeColoringPolicy;
    }

    public void setSpecialVertexPositionPolicy(int specialVertexPositionPolicy)
    {
        if (specialVertexPositionPolicy >= 0 && specialVertexPositionPolicy <= 1)
        {
            this.specialVertexPositionPolicy = specialVertexPositionPolicy;
        }
        else
        {
            System.err.println("WARNING: unknown special vertex position policy value " + specialVertexPositionPolicy);
        }
    }

    public int getSpecialVertexPositionPolicy()
    {
        return specialVertexPositionPolicy;
    }

    public void setSpecialVertexColoringPolicy(int specialVertexColoringPolicy)
    {
        if (specialVertexColoringPolicy >= 0 && specialVertexColoringPolicy <= 1)
        {
            this.specialVertexColoringPolicy = specialVertexColoringPolicy;
        }
        else
        {
            System.err.println("WARNING: unknown special node coloring policy value " + specialVertexColoringPolicy);
        }
    }

    public int getSpecialVertexColoringPolicy()
    {
        return specialVertexColoringPolicy;
    }
}
