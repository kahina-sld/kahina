package org.kahina.core.data.chart;

import java.util.Set;

import org.kahina.core.data.KahinaDataHandlingMethod;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.io.database.DatabaseHandler;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public abstract class KahinaChart extends KahinaObject
{
    // not used by KahinaDbChart
    private int nextEdgeID = 0;

    public void initialize()
    {
        setLeftBound(0);
        setRightBound(0);
        setLeftmostCovered(Integer.MAX_VALUE);
        setRightmostCovered(0);
    }

    // TODO The following four methods appear unused.
    public abstract int getLeftBound();

    public abstract void setLeftBound(int leftBound);

    public abstract int getRightBound();

    public abstract void setRightBound(int rightBound);

    // TODO Should the user of a chart really have to set leftmost covered and
    // rightmost covered herself?
    public abstract int getLeftmostCovered();

    public abstract void setLeftmostCovered(int leftBound);

    public abstract int getRightmostCovered();

    public abstract void setRightmostCovered(int rightBound);

    public int addEdge(int left, int right, String caption, int status)
    {
        int id = getNextEdgeID();
        setLeftBoundForEdge(id, left);
        setRightBoundForEdge(id, right);
        setEdgeStatus(id, status);
        setEdgeCaption(id, caption);
        System.err.println("KahinaChart.addEdge(" + left + "," + right + "," + caption + "," + status + ")");
        return id;
    }

    public abstract void removeEdge(int edgeID);

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

    protected int getNextEdgeID()
    {
        return nextEdgeID++;
    }
    
    public abstract void addEdgeDependency(int motherID, int daughterID);
    
    public abstract Set<Integer> getMotherEdgesForEdge(int id);
    
    public abstract Set<Integer> getDaughterEdgesForEdge(int id);
    
    public String exportXML(boolean asFile)
    {
        StringBuilder b = new StringBuilder("");
        if (asFile) b.append("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n");
        b.append("<chart>\n");
        for (Integer id : getSegmentsWithCaption())
        {
            b.append("  <segment ");
            b.append("id=\"" + id + "\" ");
            b.append("caption=\"" + getSegmentCaption(id) + "\" ");
            b.append("/>\n");
        }
        for (Integer id : getEdgeIDs())
        {
            b.append("  <edge ");
            b.append("id=\"" + id + "\" ");
            b.append("left=\"" + getLeftBoundForEdge(id) + "\" ");
            b.append("right=\"" + getRightBoundForEdge(id) + "\" ");
            b.append("status=\"" + getEdgeStatus(id) + "\" ");
            b.append("caption=\"" + getEdgeCaption(id) + "\" ");
            b.append("/>\n");
        }
        b.append("</chart>");
        return b.toString();
    }
    
    public static KahinaChart importXML(Document dom, int dataHandlingMethod, DatabaseHandler db)
    {
        KahinaChart m;
        if (dataHandlingMethod == KahinaDataHandlingMethod.MEMORY)
        {
            m = new KahinaMemChart();
        }
        else
        {
            m = new KahinaDbChart();
        }
        NodeList segments = dom.getElementsByTagName("segment");
        for (int i = 0; i < segments.getLength(); i++)
        {
            Element segment = (Element) segments.item(i);
            int id = Integer.parseInt(segment.getAttribute("id"));
            m.setSegmentCaption(id,segment.getAttribute("caption"));
        }
        NodeList edges = dom.getElementsByTagName("edge");
        for (int i = 0; i < edges.getLength(); i++)
        {
            Element edge = (Element) edges.item(i);
            int id = Integer.parseInt(edge.getAttribute("id"));
            int left = Integer.parseInt(edge.getAttribute("left"));
            int right = Integer.parseInt(edge.getAttribute("right"));
            int status = Integer.parseInt(edge.getAttribute("status"));
            if (left < m.getLeftmostCovered()) m.setLeftmostCovered(left);
            if (right > m.getRightmostCovered()) m.setRightmostCovered(right);
            m.setLeftBoundForEdge(id, left);
            m.setRightBoundForEdge(id, right);
            m.setEdgeStatus(id, status);
            m.setEdgeCaption(id,edge.getAttribute("caption"));
        }
        return m;
    }
}
