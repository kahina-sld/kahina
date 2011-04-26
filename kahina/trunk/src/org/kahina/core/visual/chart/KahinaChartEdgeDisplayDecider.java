package org.kahina.core.visual.chart;

import java.util.HashSet;
import java.util.Set;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class KahinaChartEdgeDisplayDecider
{
    KahinaChartView view;
    
    protected Set<String> possibleEdgeLabels;
    
    protected Set<String> displayedEdgeLabels; 
    protected Set<String> hiddenEdgeLabels;
    protected Set<Integer> displayedEdgeStatus;
    protected Set<Integer> hiddenEdgeStatus;
    
    //true means: edge label decides if draw; false: edge status decides
    boolean edgeLabelPriority;
    
    public KahinaChartEdgeDisplayDecider()
    {
        view = null;
        possibleEdgeLabels = new HashSet<String>();
        displayedEdgeLabels = new HashSet<String>();
        hiddenEdgeLabels = new HashSet<String>();
        displayedEdgeStatus = new HashSet<Integer>();
        hiddenEdgeStatus = new HashSet<Integer>();  
        edgeLabelPriority = false;
    }
    
    public void setChartView(KahinaChartView view)
    {
        this.view = view;
    }
    
    public Set<String> getPossibleEdgeLabels()
    {
        return possibleEdgeLabels;
    }
    
    public void updatePossibleEdgeLabels()
    {
        for (int edgeID : view.getModel().getEdgeIDs())
        {
        	String edgeCaption = view.getEdgeCaption(edgeID);
            if (possibleEdgeLabels.add(edgeCaption))
            {
            	hiddenEdgeLabels.add(edgeCaption);
            }
        }
    }
    
    public void swapLabelDisplay(String edgeLabel)
    {
        if (isHiddenLabel(edgeLabel))
        {
            hiddenEdgeLabels.remove(edgeLabel);
            displayedEdgeLabels.add(edgeLabel);
        }
        else
        {
            displayedEdgeLabels.remove(edgeLabel);
            hiddenEdgeLabels.add(edgeLabel);
        }       
    }
    
    public boolean isDisplayedLabel(String edgeLabel)
    {
        return displayedEdgeLabels.contains(edgeLabel);
    }
    
    public boolean isHiddenLabel(String edgeLabel)
    {
        return hiddenEdgeLabels.contains(edgeLabel);
    }
    
    public boolean decideEdgeDisplay(int edgeID)
    {
        int edgeStatus = view.getModel().getEdgeStatus(edgeID);
        String edgeLabel = view.getEdgeCaption(edgeID);
        int decision = 0;
        if (displayedEdgeLabels.contains(edgeLabel) || displayedEdgeStatus.contains(edgeStatus))
        {
            decision++;
        }
        if (hiddenEdgeLabels.contains(edgeLabel) || hiddenEdgeStatus.contains(edgeStatus))
        {
            decision--;
        }
        if (decision == 0)
        {
            if (edgeLabelPriority)
            {
                if (displayedEdgeLabels.contains(edgeLabel))
                {
                    decision++;
                }
                if (hiddenEdgeLabels.contains(edgeLabel))
                {
                    decision--;
                } 
            }
            else
            {
                if (displayedEdgeStatus.contains(edgeStatus))
                {
                    decision++;
                }
                if (hiddenEdgeStatus.contains(edgeStatus))
                {
                    decision--;
                } 
            }
        }
        return (decision > 0);
    }
    
	public Element exportXML(Document dom)
	{
		Element el = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:displaydecider");
		el.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:type","org.kahina.core.visual.tree.KahinaChart");
		
		Element possEdgeLabelsEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:stringset");
		possEdgeLabelsEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","possEdgeLabels");
		for (String s : possibleEdgeLabels)
		{
			Element sEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:string");
			sEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",s);
			possEdgeLabelsEl.appendChild(sEl);
		}
		el.appendChild(possEdgeLabelsEl);
		
		Element dispEdgeLabelsEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:stringset");
		dispEdgeLabelsEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","dispEdgeLabels");
		for (String s : displayedEdgeLabels)
		{
			Element sEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:string");
			sEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",s);
			dispEdgeLabelsEl.appendChild(sEl);
		}
		el.appendChild(dispEdgeLabelsEl);
		
		Element hideEdgeLabelsEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:stringset");
		hideEdgeLabelsEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","hideEdgeLabels");
		for (String s : hiddenEdgeLabels)
		{
			Element sEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:string");
			sEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",s);
			hideEdgeLabelsEl.appendChild(sEl);
		}
		el.appendChild(hideEdgeLabelsEl);
		
		Element dispEdgeStatusEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:intset");
		dispEdgeStatusEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","dispEdgeStatus");
		for (String s : displayedEdgeLabels)
		{
			Element sEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:int");
			sEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",s);
			dispEdgeStatusEl.appendChild(sEl);
		}
		el.appendChild(dispEdgeStatusEl);
		
		Element hideEdgeStatusEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:intset");
		hideEdgeStatusEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","hideEdgeStatus");
		for (String s : hiddenEdgeLabels)
		{
			Element sEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:int");
			sEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",s);
			hideEdgeStatusEl.appendChild(sEl);
		}
		el.appendChild(hideEdgeStatusEl);	
		
		Element edgeLabelPriorityEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:option");
		edgeLabelPriorityEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:name","edgeLabelPriority");
		edgeLabelPriorityEl.setAttributeNS("http://www.kahina.org/xml/kahina","kahina:value",edgeLabelPriority + "");	
		el.appendChild(edgeLabelPriorityEl);
		
		return el;
	}
}

