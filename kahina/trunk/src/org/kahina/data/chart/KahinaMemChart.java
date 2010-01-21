package org.kahina.data.chart;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Set;

import org.kahina.data.KahinaMemObject;

public class KahinaMemChart extends KahinaChart implements KahinaMemObject
{
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
    
    public KahinaMemChart()
    {
        super();
        segmentCaptions = new HashMap<Integer, String>();
        leftBounds = new HashMap<Integer, Integer>();
        rightBounds = new HashMap<Integer, Integer>();
        edgeCaptions = new HashMap<Integer, String>();
        status = new HashMap<Integer, Integer>();
    }
    
    public int getLeftBound()
    {
        return leftBound;
    }

    public void setLeftBound(int leftBound)
    {
        this.leftBound = leftBound;
    }

    public int getRightBound()
    {
        return rightBound;
    }

    public void setRightBound(int rightBound)
    {
        this.rightBound = rightBound;
    }

    public int getLeftmostCovered()
    {
        return leftmostCovered;
    }
    
    public void setLeftmostCovered(int leftmostCovered)
    {
        this.leftmostCovered = leftmostCovered;
    }

    public int getRightmostCovered()
    {
        return rightmostCovered;
    }
    
    public void setRightmostCovered(int rightmostCovered)
    {
        this.rightmostCovered = rightmostCovered;
    }
    
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
    
    public void setLeftBoundForEdge(int edgeID, int leftBound)
    {
        leftBounds.put(edgeID, leftBound);
    }
    
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
    
    public void setRightBoundForEdge(int edgeID, int rightBound)
    {
        rightBounds.put(edgeID, rightBound);
    }
    
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
    
    public void setEdgeStatus(int edgeID, int status)
    {
        this.status.put(edgeID, status);
    }
    
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
    
    public void setEdgeCaption(int edgeID, String edgeCaption)
    {
        edgeCaptions.put(edgeID, edgeCaption);
    }
    
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
    
    public void setSegmentCaption(int segmentID, String segmentCaption)
    {
        segmentCaptions.put(segmentID, segmentCaption);
    }
    
    public Set<Integer> getSegmentsWithCaption()
    {
        return segmentCaptions.keySet();
    }
    
    public Iterable<Integer> getEdgeIDs()
    {
        ArrayList<Integer> idList = new ArrayList<Integer>();
        idList.addAll(leftBounds.keySet());
        Collections.sort(idList);
        return idList;
    }
    
    public boolean segmentIsCovered(int id)
    {
        return (leftmostCovered <= id && id <= rightmostCovered);
    }
    
    public boolean segmentHasCaption(int id)
    {
        String segmentCaption = segmentCaptions.get(id);
        return (segmentCaption != null);
    }
}
