package org.kahina.core.visual.chart;

import java.util.HashSet;
import java.util.Set;

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
}

