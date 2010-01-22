package org.kahina.data.chart;

import org.kahina.data.*;
import org.w3c.dom.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Set;

public abstract class KahinaChart extends KahinaObject
{ 
    static int lastID = 0;
    static String type = "KahinaChart";
    
    public KahinaChart()
    {
        super(lastID++);
    }
    
    public void initialize()
    {
        setLeftBound(0);
        setRightBound(0);
        setLeftmostCovered(Integer.MAX_VALUE);
        setRightmostCovered(0);
    }

    public abstract int getLeftBound();
    public abstract void setLeftBound(int leftBound);

    public abstract int getRightBound();
    public abstract void setRightBound(int rightBound);

    public abstract int getLeftmostCovered();
    public abstract void setLeftmostCovered(int leftBound);
    
    public abstract int getRightmostCovered();
    public abstract void setRightmostCovered(int rightBound);
    
    public abstract int getLeftBoundForEdge(int edgeID);  
    public abstract void setLeftBoundForEdge(int edgeID, int leftBound);
    
    public abstract int getRightBoundForEdge(int edgeID);
    public abstract void setRightBoundForEdge(int edgeID, int rightBound);
    
    public abstract int getEdgeStatus(int edgeID);
    public abstract void setEdgeStatus(int edgeID, int status);
    
    public abstract String getEdgeCaption(int edgeID);
    public abstract void setEdgeCaption(int edgeID, String edgeCaption);
    
    public abstract String getSegmentCaption(int segmentID);
    public abstract void setSegmentCaption(int segmentID, String segmentCaption);
    
    public abstract Set<Integer> getSegmentsWithCaption();   
    public abstract Iterable<Integer> getEdgeIDs();
    
    public boolean segmentIsCovered(int id)
    {
        return (getLeftmostCovered() <= id && id <= getRightmostCovered());
    }
    
    public abstract boolean segmentHasCaption(int id);
}
