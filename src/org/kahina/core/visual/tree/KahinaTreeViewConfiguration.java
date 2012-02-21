package org.kahina.core.visual.tree;

import java.awt.Color;

import org.kahina.core.io.util.XMLUtil;
import org.kahina.core.visual.KahinaViewConfiguration;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class KahinaTreeViewConfiguration extends KahinaViewConfiguration
{
	// display options
	private int horizontalDistance = 5;
	private int verticalDistance = 25;
	private Color bgColor = Color.WHITE;
	private int nodeShapePolicy = KahinaTreeViewOptions.BOX_SHAPE;
    private int edgeTagPolicy = KahinaTreeViewOptions.OVAL_EDGE_TAGS;
	private int nodeDisplayPolicy = KahinaTreeViewOptions.STATUS_DEFAULT_YES;
	private int collapsePolicy = KahinaTreeViewOptions.COLLAPSE_SECONDARY;
	private int terminalsPolicy = KahinaTreeViewOptions.NO_SPECIAL_TREATMENT;
	private int lineShapePolicy = KahinaTreeViewOptions.INVISIBLE_LINES;
	private int secondaryLineShapePolicy = KahinaTreeViewOptions.EDGY_LINES;
	private int nodePositionPolicy = KahinaTreeViewOptions.LEFT_ALIGNED_NODES;
	private int antialiasingPolicy = KahinaTreeViewOptions.ANTIALIASING;
	private int displayOrientation = KahinaTreeViewOptions.TOP_DOWN_DISPLAY;
    private int cuttingPolicy = KahinaTreeViewOptions.SECONDARY_CUT;
    private int autoscrollPolicy = KahinaTreeViewOptions.AUTOSCROLL_TO_MARKED_NODE;
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
		} 
		else
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
			System.err.println("WARNING: TreeView received null as background color! Defaulting to Color.WHITE!");
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
	
    public int getNodeShapePolicy()
    {
        return nodeShapePolicy;
    }

    public void setNodeShapePolicy(int newPolicy)
    {
        if (newPolicy >= 0 && newPolicy <= 1)
        {
            nodeShapePolicy = newPolicy;
        } 
        else
        {
            System.err.println("WARNING: unknown node shape policy value " + newPolicy);
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
	
    public int getAutoscrollPolicy()
    {
        return autoscrollPolicy;
    }

    public void setAutoscrollPolicy(int autoscrollPolicy)
    {
        if (autoscrollPolicy >= 0 && autoscrollPolicy <= 1)
        {
            this.autoscrollPolicy = autoscrollPolicy;
        } 
        else
        {
            System.err.println("WARNING: unknown autoscroll policy value " + autoscrollPolicy);
        }
    }
    
    public int getEdgeTagPolicy()
    {
        return edgeTagPolicy;
    }

    public void setEdgeTagPolicy(int edgeTagPolicy)
    {
        if (edgeTagPolicy >= 0 && edgeTagPolicy <= 3)
        {
            this.edgeTagPolicy = edgeTagPolicy;
        } 
        else
        {
            System.err.println("WARNING: unknown edge tag policy value " + edgeTagPolicy);
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

	public int getTerminalsPolicy()
	{
		return terminalsPolicy;
	}

	public void setTerminalsPolicy(int newPolicy)
	{
		if (newPolicy >= 0 && newPolicy <= 2)
		{
			terminalsPolicy = newPolicy;
		} 
		else
		{
			System.err.println("WARNING: unknown terminals policy value " + newPolicy);
		}
	}
	
	public static KahinaTreeViewConfiguration importXML(Element configEl)
	{
		KahinaTreeViewConfiguration config = new KahinaTreeViewConfiguration();
		Element optionEl = null;
		NodeList optionsList = configEl.getElementsByTagName("kahina:option");
		for (int i = 0; i < optionsList.getLength(); i++)
		{
			optionEl = (Element) optionsList.item(i);
			String optName = XMLUtil.attrStrVal(optionEl, "kahina:name");
			if (optName.equals("horizontalDistance"))
			{
				config.horizontalDistance = XMLUtil.attrIntVal(optionEl, "kahina:value");
			}
			else if (optName.equals("verticalDistance"))
			{
				config.verticalDistance = XMLUtil.attrIntVal(optionEl, "kahina:value");
			}
			else if (optName.equals("fontSize"))
			{
				config.fontSize = XMLUtil.attrIntVal(optionEl, "kahina:value");
			}
			else if (optName.equals("displaySecondDimension"))
			{
				config.displaySecondDimension = XMLUtil.attrBoolVal(optionEl, "kahina:value");
			}
			else if (optName.equals("bgColor"))
			{
				config.bgColor = XMLUtil.attrColorVal(optionEl, "kahina:value");
			}
			else if (optName.equals("nodeShapePolicy"))
			{
				config.nodeShapePolicy = XMLUtil.attrIntVal(optionEl, "kahina:value");
			}
			else if (optName.equals("edgeTagPolicy"))
			{
				config.edgeTagPolicy = XMLUtil.attrIntVal(optionEl, "kahina:value");
			}
			else if (optName.equals("nodeDisplayPolicy"))
			{
				config.nodeDisplayPolicy = XMLUtil.attrIntVal(optionEl, "kahina:value");
			}
			else if (optName.equals("collapsePolicy"))
			{
				config.collapsePolicy = XMLUtil.attrIntVal(optionEl, "kahina:value");
			}
			else if (optName.equals("terminalsPolicy"))
			{
				config.terminalsPolicy = XMLUtil.attrIntVal(optionEl, "kahina:value");
			}
			else if (optName.equals("lineShapePolicy"))
			{
				config.lineShapePolicy = XMLUtil.attrIntVal(optionEl, "kahina:value");
			}
			else if (optName.equals("secondaryLineShapePolicy"))
			{
				config.secondaryLineShapePolicy = XMLUtil.attrIntVal(optionEl, "kahina:value");
			}
			else if (optName.equals("nodePositionPolicy"))
			{
				config.nodePositionPolicy = XMLUtil.attrIntVal(optionEl, "kahina:value");
			}
			else if (optName.equals("antialiasingPolicy"))
			{
				config.antialiasingPolicy = XMLUtil.attrIntVal(optionEl, "kahina:value");
			}
			else if (optName.equals("displayOrientation"))
			{
				config.displayOrientation = XMLUtil.attrIntVal(optionEl, "kahina:value");
			}
			else if (optName.equals("cuttingPolicy"))
			{
				config.cuttingPolicy = XMLUtil.attrIntVal(optionEl, "kahina:value");
			}
		}
		return config;
	}
	
	public Element exportXML(Document dom)
	{
		Element el = super.exportXML(dom);
		el.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:type","org.kahina.core.visual.tree.KahinaTreeViewConfiguration");
		
		Element horDistEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		horDistEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","horizontalDistance");
		horDistEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",horizontalDistance + "");	
		el.appendChild(horDistEl);
		
		Element verDistEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		verDistEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","verticalDistance");
		verDistEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",verticalDistance + "");	
		el.appendChild(verDistEl);
		
		Element fontSizeEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		fontSizeEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","fontSize");
		fontSizeEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",fontSize + "");	
		el.appendChild(fontSizeEl);
		
		Element displaySecondDimEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		displaySecondDimEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","displaySecondDimension");
		displaySecondDimEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",displaySecondDimension + "");	
		el.appendChild(displaySecondDimEl);
		
		Element bgColorEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		bgColorEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","bgColor");
		bgColorEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value","(" + bgColor.getRed() + "," + bgColor.getGreen() + "," + bgColor.getBlue() + ")");	
		el.appendChild(bgColorEl);
		
		Element nodeShapePolicyEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		nodeShapePolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","nodeShapePolicy");
		nodeShapePolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",nodeShapePolicy + "");	
		el.appendChild(nodeShapePolicyEl);
		
		Element edgeShapePolicyEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		edgeShapePolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","edgeTagPolicy");
		edgeShapePolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",edgeTagPolicy + "");	
		el.appendChild(edgeShapePolicyEl);
		
		Element nodeDisplayPolicyEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		nodeDisplayPolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","nodeDisplayPolicy");
		nodeDisplayPolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",nodeDisplayPolicy + "");	
		el.appendChild(nodeDisplayPolicyEl);
		
		Element collapsePolicyEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		collapsePolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","collapsePolicy");
		collapsePolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",collapsePolicy + "");	
		el.appendChild(collapsePolicyEl);
		
		Element terminalsPolicyEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		terminalsPolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","terminalsPolicy");
		terminalsPolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",terminalsPolicy + "");	
		el.appendChild(terminalsPolicyEl);
		
		Element lineShapePolicyEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		lineShapePolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","lineShapePolicy");
		lineShapePolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",lineShapePolicy + "");	
		el.appendChild(lineShapePolicyEl);
		
		Element secondaryLineShapePolicyEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		secondaryLineShapePolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","secondaryLineShapePolicy");
		secondaryLineShapePolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",secondaryLineShapePolicy + "");	
		el.appendChild(secondaryLineShapePolicyEl);
		
		Element nodePositionPolicyEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		nodePositionPolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","nodePositionPolicy");
		nodePositionPolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",nodePositionPolicy + "");	
		el.appendChild(nodePositionPolicyEl);
		
		Element antialiasingPolicyEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		antialiasingPolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","antialiasingPolicy");
		antialiasingPolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",antialiasingPolicy + "");	
		el.appendChild(antialiasingPolicyEl);
		
		Element displayOrientationEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		displayOrientationEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","displayOrientation");
		displayOrientationEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",displayOrientation + "");	
		el.appendChild(displayOrientationEl);
		
		Element cuttingPolicyEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		cuttingPolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","cuttingPolicy");
		cuttingPolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",cuttingPolicy + "");	
		el.appendChild(cuttingPolicyEl);
		
		return el;
	}
}
