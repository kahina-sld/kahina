package org.kahina.core.visual.tree;

import java.awt.Color;
import java.util.HashMap;

import org.kahina.core.visual.KahinaViewConfiguration;

public class KahinaTreeViewConfiguration extends KahinaViewConfiguration<KahinaTreeView>
{
	// display options
	private int horizontalDistance = 5;
	private int verticalDistance = 25;
	Color bgColor = Color.WHITE;
	private int nodeShapePolicy = KahinaTreeViewOptions.BOX_SHAPE;
	private int edgeShapePolicy = KahinaTreeViewOptions.OVAL_SHAPE;
	private int nodeDisplayPolicy = KahinaTreeViewOptions.STATUS_DEFAULT_YES;
	private int collapsePolicy = KahinaTreeViewOptions.COLLAPSE_SECONDARY;
	private int terminalsPolicy = KahinaTreeViewOptions.NO_SPECIAL_TREATMENT;
	private int lineShapePolicy = KahinaTreeViewOptions.INVISIBLE_LINES;
	private int secondaryLineShapePolicy = KahinaTreeViewOptions.EDGY_LINES;
	private int nodePositionPolicy = KahinaTreeViewOptions.LEFT_ALIGNED_NODES;
	private int antialiasingPolicy = KahinaTreeViewOptions.ANTIALIASING;
	private int displayOrientation = KahinaTreeViewOptions.TOP_DOWN_DISPLAY;
    private int cuttingPolicy = KahinaTreeViewOptions.SECONDARY_CUT;
	private boolean displaySecondDimension = true;
	private int fontSize; // also determines zoom factor
	
	public KahinaTreeViewConfiguration()
	{
		horizontalDistance = 10;
		verticalDistance = 2;
		fontSize = 10;
	}
	
	public void zoomIn()
	{
		if (fontSize < 30)
		{
			fontSize += 1;
		} else
		{
			System.err.println("No zoom levels beyond 30 allowed!");
		}
	}

	public void zoomOut()
	{
		if (fontSize > 6)
		{
			fontSize -= 1;
		} else
		{
			System.err.println("No zoom levels below 6 allowed!");
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
	
	public void setBackgroundColor(Color bgColor)
	{
		if (bgColor == null)
		{
			System.err.println("WARNING: TreeView recieved null as background color! Defaulting to Color.WHITE!");
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
		} else
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

	public boolean isSecondDimensionDisplayed()
	{
		return displaySecondDimension;
	}

	public void toggleSecondDimensionDisplay()
	{
		displaySecondDimension = !displaySecondDimension;
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

	public int getCollapsePolicy()
	{
		return collapsePolicy;
	}

	public void setCollapsePolicy(int collapsePolicy)
	{
		if (collapsePolicy >= 0 && collapsePolicy <= 2)
		{
			this.collapsePolicy = collapsePolicy;
		} else
		{
			System.err.println("WARNING: unknown collapse policy value " + collapsePolicy);
		}
	}

	public int getDisplayOrientation()
	{
		return displayOrientation;
	}

	public void setDisplayOrientation(int newPolicy)
	{
		if (newPolicy >= 0 && newPolicy <= 1)
		{
			displayOrientation = newPolicy;
		} else
		{
			System.err.println("WARNING: unknown displayOrientation value " + newPolicy);
		}
	}

	public int getEdgeShapePolicy()
	{
		return edgeShapePolicy;
	}

	public void setEdgeShapePolicy(int newPolicy)
	{
		if (newPolicy >= 0 && newPolicy <= 1)
		{
			edgeShapePolicy = newPolicy;
		} else
		{
			System.err.println("WARNING: unknown edge shape policy value " + newPolicy);
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

	public int getLineShapePolicy()
	{
		return lineShapePolicy;
	}

	public void setLineShapePolicy(int newPolicy)
	{
		if (newPolicy >= 0 && newPolicy <= 2)
		{
			lineShapePolicy = newPolicy;
		} else
		{
			System.err.println("WARNING: unknown line shape policy value " + newPolicy);
		}
	}

	public int getSecondaryLineShapePolicy()
	{
		return secondaryLineShapePolicy;
	}

	public void setSecondaryLineShapePolicy(int newPolicy)
	{
		if (newPolicy >= 0 && newPolicy <= 2)
		{
			secondaryLineShapePolicy = newPolicy;
		} else
		{
			System.err.println("WARNING: unknown line shape policy value " + newPolicy);
		}
	}

	public int getNodeDisplayPolicy()
	{
		return nodeDisplayPolicy;
	}

	public void setNodeDisplayPolicy(int newPolicy)
	{
		if (newPolicy >= 0 && newPolicy <= 4)
		{
			nodeDisplayPolicy = newPolicy;
		} else
		{
			System.err.println("WARNING: unknown node display policy value " + newPolicy);
		}
	}

	public int getNodePositionPolicy()
	{
		return nodePositionPolicy;
	}

	public void setNodePositionPolicy(int newPolicy)
	{
		if (newPolicy >= 0 && newPolicy <= 2)
		{
			nodePositionPolicy = newPolicy;
		} else
		{
			System.err.println("WARNING: unknown node position policy value " + newPolicy);
		}
	}

	public int getNodeShapePolicy()
	{
		return nodeShapePolicy;
	}

	public void setNodeShapePolicy(int newPolicy)
	{
		if (newPolicy >= 0 && newPolicy <= 1)
		{
			nodeShapePolicy = newPolicy;
		} else
		{
			System.err.println("WARNING: unknown node shape policy value " + newPolicy);
		}
	}

	public int getTerminalsPolicy()
	{
		return terminalsPolicy;
	}

	public void setTerminalsPolicy(int newPolicy)
	{
		if (newPolicy >= 0 && newPolicy <= 2)
		{
			terminalsPolicy = newPolicy;
		} else
		{
			System.err.println("WARNING: unknown terminals policy value " + newPolicy);
		}
	}
}
