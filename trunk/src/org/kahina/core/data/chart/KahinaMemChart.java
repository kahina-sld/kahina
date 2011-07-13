package org.kahina.core.data.chart;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

public class KahinaMemChart extends KahinaChart
{
	/**
	 * 
	 */
	private static final long serialVersionUID = 8981755950162448512L;

	private static final boolean verbose = false;
	
    //the chart is divided up into cells (vertical segments) that the edges can range over
    //leftmost: leftBound, rightmost: rightBound
    
    //minimum and maximum values for chart ranges
    int leftBound;
    int rightBound;
    
    //contains information on highest and lowest ranges; important for centered display
    int leftmostCovered;
    int rightmostCovered;
    
    //store captions for segment
    HashMap<Integer, String> segmentCaptions;
    
    //encode properties of individual edges
    HashMap<Integer, Integer> leftBounds;
    HashMap<Integer, Integer> rightBounds;
    HashMap<Integer, String> edgeCaptions; //captions are displayed on the edges
    HashMap<Integer, Integer> status; //can be used to encode different types
    HashMap<Integer, Set<Integer>> motherEdges;
    HashMap<Integer, Set<Integer>> daughterEdges;   
    
    public KahinaMemChart()
    {
        super();
        segmentCaptions = new HashMap<Integer, String>();
        leftBounds = new HashMap<Integer, Integer>();
        rightBounds = new HashMap<Integer, Integer>();
        edgeCaptions = new HashMap<Integer, String>();
        status = new HashMap<Integer, Integer>();
        motherEdges = new HashMap<Integer, Set<Integer>>();
        daughterEdges = new HashMap<Integer, Set<Integer>>();
    }
    
    @Override
	public int getLeftBound()
    {
        return leftBound;
    }

    @Override
	public void setLeftBound(int leftBound)
    {
        this.leftBound = leftBound;
    }

    @Override
	public int getRightBound()
    {
        return rightBound;
    }

    @Override
	public void setRightBound(int rightBound)
    {
        this.rightBound = rightBound;
    }

    @Override
	public int getLeftmostCovered()
    {
        return leftmostCovered;
    }
    
    @Override
	public void setLeftmostCovered(int leftmostCovered)
    {
        this.leftmostCovered = leftmostCovered;
    }

    @Override
	public int getRightmostCovered()
    {
        return rightmostCovered;
    }
    
    @Override
	public void setRightmostCovered(int rightmostCovered)
    {
        this.rightmostCovered = rightmostCovered;
    }
    
    @Override
	public void removeEdge(int edgeID)
    {
        leftBounds.remove(edgeID);
        rightBounds.remove(edgeID);
        edgeCaptions.remove(edgeID);
        status.remove(edgeID);
    }
    
    @Override
	public int getLeftBoundForEdge(int edgeID)
    {
        Integer leftBound = leftBounds.get(edgeID);
        if (leftBound == null)
        {
            return -1;
        }
        else
        {
            return leftBound;
        }
    }
    
    @Override
	public void setLeftBoundForEdge(int edgeID, int leftBound)
    {
        leftBounds.put(edgeID, leftBound);
    }
    
    @Override
	public int getRightBoundForEdge(int edgeID)
    {
        Integer rightBound = rightBounds.get(edgeID);
        if (rightBound == null)
        {
            return -1;
        }
        else
        {
            return rightBound;
        }
    }
    
    @Override
	public void setRightBoundForEdge(int edgeID, int rightBound)
    {
    	if (verbose)
    	{
    		System.err.println(this + ".setRightBoundForEdge(" + edgeID + ", " + rightBound + ")");
    	}
        rightBounds.put(edgeID, rightBound);
    	if (verbose)
    	{
    		System.err.println("//" + this + ".setRightBoundForEdge(" + edgeID + ", " + rightBound + ")");
    	}
    }
    
    @Override
	public int getEdgeStatus(int edgeID)
    {
        Integer edgeStatus = status.get(edgeID);
        if (edgeStatus == null)
        {
            return -1;
        }
        else
        {
            return edgeStatus;
        }
    }
    
    @Override
	public void setEdgeStatus(int edgeID, int status)
    {
        this.status.put(edgeID, status);
    }
    
    @Override
	public String getEdgeCaption(int edgeID)
    {
        String edgeCaption = edgeCaptions.get(edgeID);
        if (edgeCaption == null)
        {
            return "";
        }
        else
        {
            return edgeCaption;
        }
    }
    
    @Override
	public void setEdgeCaption(int edgeID, String edgeCaption)
    {
        edgeCaptions.put(edgeID, edgeCaption);
    }
    
    @Override
	public String getSegmentCaption(int segmentID)
    {
        String segmentCaption = segmentCaptions.get(segmentID);
        if (segmentCaption == null)
        {
            return "";
        }
        else
        {
            return segmentCaption;
        }
    }
    
    @Override
	public void setSegmentCaption(int segmentID, String segmentCaption)
    {
        segmentCaptions.put(segmentID, segmentCaption);
    }
    
    @Override
	public Set<Integer> getSegmentsWithCaption()
    {
        return segmentCaptions.keySet();
    }
    
    @Override
	public Iterable<Integer> getEdgeIDs()
    {
        ArrayList<Integer> idList = new ArrayList<Integer>();
        idList.addAll(leftBounds.keySet());
        Collections.sort(idList);
        return idList;
    }
    
    @Override
	public boolean segmentHasCaption(int id)
    {
        String segmentCaption = segmentCaptions.get(id);
        return (segmentCaption != null);
    }
    
    @Override
	public void addEdgeDependency(int motherID, int daughterID)
    {
    	if (verbose)
    	{
    		System.err.println(this + ".addEdgeDependency(" + motherID + ", " + daughterID + ")");
    	}
        Set<Integer> daughters = daughterEdges.get(motherID);
        if (daughters == null)
        {
            daughters = new HashSet<Integer>();
            daughterEdges.put(motherID, daughters);
        }
        daughters.add(daughterID);
        
        Set<Integer> mothers = motherEdges.get(daughterID);
        if (mothers == null)
        {
            mothers = new HashSet<Integer>();
            motherEdges.put(daughterID, mothers);
        }
        mothers.add(motherID);
    }
    
    @Override
	public Set<Integer> getMotherEdgesForEdge(int daughterID)
    {
    	if (verbose)
    	{
    		System.err.print(this + ".getMotherEdgesForEdge(" + daughterID + ")=");
    	}
        Set<Integer> mothers = motherEdges.get(daughterID);
        if (mothers == null) mothers = new HashSet<Integer>();
        if (verbose)
        {
        	System.err.println(mothers);
        }
        return mothers;
    }
    
    @Override
	public Set<Integer> getDaughterEdgesForEdge(int motherID)
    {
    	if (verbose)
    	{
    		System.err.print(this + ".getDaughterEdgesForEdge(" + motherID + ")=");
    	}
        Set<Integer> daughters = daughterEdges.get(motherID);
        if (daughters == null) daughters = new HashSet<Integer>();
        if (verbose)
        {
        	System.err.println(daughters);
        }
        return daughters;
    }
}
