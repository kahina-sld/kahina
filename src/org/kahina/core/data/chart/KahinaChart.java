package org.kahina.core.data.chart;

import java.io.File;
import java.io.IOException;
import java.util.Set;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.kahina.core.data.KahinaObject;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

public abstract class KahinaChart extends KahinaObject
{

	/**
	 * 
	 */
	private static final long serialVersionUID = -5878453631395737334L;

	private static final boolean verbose = false;
	
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

	public abstract int addEdge(int left, int right, String caption, int status);
	
	public abstract void addEdge(int id, int left, int right, String caption, int status);

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
    
    public abstract Set<Integer> getDependencyRoots();

	public abstract Set<Integer> getMotherEdgesForEdge(int id);

	public abstract Set<Integer> getDaughterEdgesForEdge(int id);

	public String exportXML(boolean asFile)
	{
		StringBuilder b = new StringBuilder("");
		if (asFile)
			b.append("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n");
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
        for (Integer motherID : getEdgeIDs())
        {
            for (Integer daughterID : getDaughterEdgesForEdge(motherID))
            {
                b.append(" <dependency ");
                b.append("motherID=\"" + motherID + "\" ");
                b.append("daughterID=\"" + daughterID + "\" ");
                b.append("/>\n");
            }
        }
		b.append("</chart>");
		return b.toString();
	}

	public static KahinaChart importXML(String fileName)
	{
        KahinaChart m = new KahinaMemChart();
        File file = new File(fileName);
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();    
        try
        {
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document dom = db.parse(file);
    		NodeList segments = dom.getElementsByTagName("segment");
    		for (int i = 0; i < segments.getLength(); i++)
    		{
    			Element segment = (Element) segments.item(i);
    			int id = Integer.parseInt(segment.getAttribute("id"));
    			m.setSegmentCaption(id, segment.getAttribute("caption"));
    		}
    		NodeList edges = dom.getElementsByTagName("edge");
    		for (int i = 0; i < edges.getLength(); i++)
    		{
    			Element edge = (Element) edges.item(i);
    			int id = Integer.parseInt(edge.getAttribute("id"));
    			int left = Integer.parseInt(edge.getAttribute("left"));
    			int right = Integer.parseInt(edge.getAttribute("right"));
    			int status = Integer.parseInt(edge.getAttribute("status"));
    			if (left < m.getLeftmostCovered())
    				m.setLeftmostCovered(left);
    			if (right > m.getRightmostCovered())
    				m.setRightmostCovered(right);
    			m.addEdge(id, left, right, edge.getAttribute("caption"), status);
    		}
            NodeList dependencies = dom.getElementsByTagName("dependency");
            for (int i = 0; i < dependencies.getLength(); i++)
            {
                Element dependency = (Element) dependencies.item(i);
                int motherID = Integer.parseInt(dependency.getAttribute("motherID"));
                int daughterID = Integer.parseInt(dependency.getAttribute("daughterID"));
                m.addEdgeDependency(motherID, daughterID);
            }
        }
        catch (ParserConfigurationException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
            System.err.println("ERROR while parsing chart file \"" + fileName + "\". Returning the empty chart!");
        }
        catch (SAXException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
            System.err.println("ERROR while parsing chart file \"" + fileName + "\". Returning the empty chart!");
        }
        catch (IOException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
            System.err.println("ERROR while parsing chart file \"" + fileName + "\". Returning the empty chart!");
        }
		return m;
	}
}
