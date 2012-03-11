package org.kahina.core.visual.dag;

import java.awt.Color;

import org.kahina.core.visual.KahinaViewConfiguration;
import org.kahina.core.visual.tree.KahinaTreeViewOptions;

public class KahinaDAGViewConfiguration extends KahinaViewConfiguration
{
    private int zoomLevel = 10;
    private int nodeSize = 14;
    private int horizontalDistance = 5;
    private int verticalDistance = 25;
    
    private Color bgColor = Color.WHITE;
    
    private int antialiasingPolicy = KahinaTreeViewOptions.ANTIALIASING;
    
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
    
    public int getHorizontalDistance()
    {
        return horizontalDistance;
    }

    public void setHorizontalDistance(int horizontalDistance)
    {
        this.horizontalDistance = horizontalDistance;
    }

    public void decreaseHorizontalDistance()
    {
        if (horizontalDistance > 2)
        {
            horizontalDistance -= 1;
        } 
        else
        {
            System.err.println("No horizontal distance values under 2 allowed!");
        }
    }

    public void increaseHorizontalDistance()
    {
        if (horizontalDistance < 20)
        {
            horizontalDistance += 1;
        } else
        {
            System.err.println("No horizontal distance values over 20 allowed!");
        }
    }
    
    public int getVerticalDistance()
    {
        return verticalDistance;
    }

    public void setVerticalDistance(int verticalDistance)
    {
        this.verticalDistance = verticalDistance;
    }

    public void decreaseVerticalDistance()
    {
        if (verticalDistance > 2)
        {
            verticalDistance -= 1;
        } else
        {
            System.err.println("No vertical distance values under 2 allowed!");
        }
    }

    public void increaseVerticalDistance()
    {
        if (verticalDistance < 20)
        {
            verticalDistance += 1;
        } else
        {
            System.err.println("No vertical distance values over 20 allowed!");
        }
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
