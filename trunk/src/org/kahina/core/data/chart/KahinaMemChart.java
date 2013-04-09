package org.kahina.core.data.chart;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

public class KahinaMemChart extends KahinaChart
{
	/**
	 * 
	 */
	private static final long serialVersionUID = 8981755950162448512L;

	private static final boolean VERBOSE = true;
	
    //the chart is divided up into cells (vertical segments) that the edges can range over
    //leftmost: leftBound, rightmost: rightBound
    
    //minimum and maximum values for chart ranges
    int leftBound = 0;
    int rightBound = 0;
    
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
    HashMap<Integer, TreeSet<Integer>> motherEdges;
    HashMap<Integer, TreeSet<Integer>> daughterEdges;   
    
    TreeSet<Integer> dependencyRoots;
    
    public KahinaMemChart()
    {
        super();
        segmentCaptions = new HashMap<Integer, String>();
        leftBounds = new HashMap<Integer, Integer>();
        rightBounds = new HashMap<Integer, Integer>();
        edgeCaptions = new HashMap<Integer, String>();
        status = new HashMap<Integer, Integer>();
        motherEdges = new HashMap<Integer, TreeSet<Integer>>();
        daughterEdges = new HashMap<Integer, TreeSet<Integer>>();
        dependencyRoots = new TreeSet<Integer>();
    }
    
    public int addEdge(int left, int right, String caption, int status)
    {
        int id = getNextEdgeID();
        dependencyRoots.add(id);
        if (right > rightBound) rightBound = right;
        setLeftBoundForEdge(id, left);
        setRightBoundForEdge(id, right);
        setEdgeStatus(id, status);
        setEdgeCaption(id, caption);
        if (VERBOSE)
        {
            System.err.println("  " + id + " = chart.addEdge(" + left + "," + right + "," + caption + "," + status + ")");
        }
        return id;
    }
    
    public void addEdge(int id, int left, int right, String caption, int status)
    {
        dependencyRoots.add(id);
        if (right > rightBound) rightBound = right;
        setLeftBoundForEdge(id, left);
        setRightBoundForEdge(id, right);
        setEdgeStatus(id, status);
        setEdgeCaption(id, caption);
        if (VERBOSE)
        {
            System.err.println("  " + id + " = KahinaChart.addEdge(" + left + "," + right + "," + caption + "," + status + ")");
        }
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
        dependencyRoots.remove(edgeID);
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
    	if (VERBOSE)
    	{
    		System.err.println("  chart.setRightBoundForEdge(" + edgeID + ", " + rightBound + ")");
    	}
        rightBounds.put(edgeID, rightBound);
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
        if (motherID < 0 || daughterID < 0)
        {
            System.err.println("ERROR: illegal argument in addEdgeDependency(" + motherID + "," + daughterID + ")");
            return;
        }
    	if (VERBOSE)
    	{
    		System.err.println("  chart.addEdgeDependency(" + motherID + ", " + daughterID + ")");
    	}
        TreeSet<Integer> daughters = daughterEdges.get(motherID);
        if (daughters == null)
        {
            daughters = new TreeSet<Integer>();
            daughterEdges.put(motherID, daughters);
        }
        daughters.add(daughterID);
        
        TreeSet<Integer> mothers = motherEdges.get(daughterID);
        if (mothers == null)
        {
            mothers = new TreeSet<Integer>();
            motherEdges.put(daughterID, mothers);
        }
        mothers.add(motherID);
        
        if (dependencyRoots.contains(daughterID))
        {
            dependencyRoots.remove(daughterID);
        }
    }
    
    @Override
	public Set<Integer> getMotherEdgesForEdge(int daughterID)
    {
    	if (VERBOSE)
    	{
    		//System.err.print(this + ".getMotherEdgesForEdge(" + daughterID + ")=");
    	}
        Set<Integer> mothers = motherEdges.get(daughterID);
        if (mothers == null) mothers = new HashSet<Integer>();
        if (VERBOSE)
        {
        	//System.err.println(mothers);
        }
        return mothers;
    }
    
    @Override
	public Set<Integer> getDaughterEdgesForEdge(int motherID)
    {
    	if (VERBOSE)
    	{
    		//System.err.print(this + ".getDaughterEdgesForEdge(" + motherID + ")=");
    	}
        Set<Integer> daughters = daughterEdges.get(motherID);
        if (daughters == null) daughters = new HashSet<Integer>();
        if (VERBOSE)
        {
        	//System.err.println(daughters);
        }
        return daughters;
    }
    
    public Set<Integer> getDependencyRoots()
    {
        return dependencyRoots;
    }
}
