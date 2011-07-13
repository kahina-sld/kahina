package org.kahina.core.visual.chart;

import java.awt.Color;

import org.kahina.core.io.util.XMLUtilities;
import org.kahina.core.visual.KahinaViewConfiguration;
import org.kahina.core.visual.tree.KahinaTreeViewConfiguration;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

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
    
	public static KahinaChartViewConfiguration importXML(Element configEl)
	{
		KahinaChartViewConfiguration config = new KahinaChartViewConfiguration();
		Element optionEl = null;
		NodeList optionsList = configEl.getElementsByTagName("kahina:option");
		for (int i = 0; i < optionsList.getLength(); i++)
		{
			optionEl = (Element) optionsList.item(i);
			String optName = XMLUtilities.attrStrVal(optionEl, "kahina:name");
			if (optName.equals("cellWidth"))
			{
				config.cellWidth = XMLUtilities.attrIntVal(optionEl, "kahina:value");
			}
			else if (optName.equals("fontSize"))
			{
				config.fontSize = XMLUtilities.attrIntVal(optionEl, "kahina:value");
			}
			else if (optName.equals("bgColor"))
			{
				config.bgColor = XMLUtilities.attrColorVal(optionEl, "kahina:value");
			}
			else if (optName.equals("cellWidthPolicy"))
			{
				config.cellWidthPolicy = XMLUtilities.attrIntVal(optionEl, "kahina:value");
			}
			else if (optName.equals("edgeStackingPolicy"))
			{
				config.edgeStackingPolicy = XMLUtilities.attrIntVal(optionEl, "kahina:value");
			}
			else if (optName.equals("displayRangePolicy"))
			{
				config.displayRangePolicy = XMLUtilities.attrIntVal(optionEl, "kahina:value");
			}
			else if (optName.equals("dependencyDisplayPolicy"))
			{
				config.dependencyDisplayPolicy = XMLUtilities.attrIntVal(optionEl, "kahina:value");
			}
			else if (optName.equals("transitiveAncestors"))
			{
				config.transitiveAncestors = XMLUtilities.attrBoolVal(optionEl, "kahina:value");
			}
			else if (optName.equals("transitiveDescendants"))
			{
				config.transitiveDescendants = XMLUtilities.attrBoolVal(optionEl, "kahina:value");
			}
			else if (optName.equals("antialiasingPolicy"))
			{
				config.antialiasingPolicy = XMLUtilities.attrIntVal(optionEl, "kahina:value");
			}
			else if (optName.equals("displayOrientation"))
			{
				config.displayOrientation = XMLUtilities.attrIntVal(optionEl, "kahina:value");
			}
			//TODO: import display decider
			config.displayDecider = new KahinaChartEdgeDisplayDecider();
		}
		return config;
	}
    
	public Element exportXML(Document dom)
	{
		Element el = super.exportXML(dom);
		el.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:type","org.kahina.core.visual.chart.KahinaChartViewConfiguration");
		
		Element cellWidthEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		cellWidthEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","cellWidth");
		cellWidthEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",cellWidth + "");	
		el.appendChild(cellWidthEl);
		
		Element fontSizeEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		fontSizeEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","fontSize");
		fontSizeEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",fontSize + "");	
		el.appendChild(fontSizeEl);
		
		Element bgColorEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		bgColorEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","bgColor");
		bgColorEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",bgColor.toString());	
		el.appendChild(bgColorEl);
		
		Element cellWidthPolicyEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		cellWidthPolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","cellWidthPolicy");
		cellWidthPolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",cellWidthPolicy + "");	
		el.appendChild(cellWidthPolicyEl);
		
		Element edgeStackingPolicyEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		edgeStackingPolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","edgeStackingPolicy");
		edgeStackingPolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",edgeStackingPolicy + "");	
		el.appendChild(edgeStackingPolicyEl);
		
		Element displayRangePolicyEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		displayRangePolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","displayRangePolicy");
		displayRangePolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",displayRangePolicy + "");	
		el.appendChild(displayRangePolicyEl);
		
		Element dependencyDisplayPolicyEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		dependencyDisplayPolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","dependencyDisplayPolicy");
		dependencyDisplayPolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",dependencyDisplayPolicy + "");	
		el.appendChild(dependencyDisplayPolicyEl);
		
		Element antialiasingPolicyEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		antialiasingPolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","antialiasingPolicy");
		antialiasingPolicyEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",antialiasingPolicy + "");	
		el.appendChild(antialiasingPolicyEl);
		
		Element displayOrientationEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		displayOrientationEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","displayOrientation");
		displayOrientationEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",displayOrientation + "");	
		el.appendChild(displayOrientationEl);
		
		Element transitiveAncestorsEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		transitiveAncestorsEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","transitiveAncestors");
		transitiveAncestorsEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",transitiveAncestors + "");	
		el.appendChild(transitiveAncestorsEl);
		
		Element transitiveDescendantsEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		transitiveDescendantsEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","transitiveDescendants");
		transitiveDescendantsEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",transitiveDescendants + "");	
		el.appendChild(transitiveDescendantsEl);
		
		el.appendChild(displayDecider.exportXML(dom));
		
		return el;
	}
}
