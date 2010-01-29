package org.kahina.io.chart;

import org.kahina.data.KahinaDataHandlingMethod;
import org.kahina.data.chart.KahinaChart;
import org.kahina.data.chart.KahinaDbChart;
import org.kahina.data.chart.KahinaMemChart;
import org.kahina.io.database.DatabaseHandler;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class KahinaChartIO
{
    public String exportXML(KahinaChart chart)
    {
        StringBuilder b = new StringBuilder("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n");
        b.append("<kahinaChart>\n");
        for (Integer id : chart.getSegmentsWithCaption())
        {
            b.append("  <segment ");
            b.append("id=\"" + id + "\" ");
            b.append("caption=\"" + chart.getSegmentCaption(id) + "\" ");
            b.append("/>\n");
        }
        for (Integer id : chart.getEdgeIDs())
        {
            b.append("  <edge ");
            b.append("id=\"" + id + "\" ");
            b.append("left=\"" + chart.getLeftBoundForEdge(id) + "\" ");
            b.append("right=\"" + chart.getRightBoundForEdge(id) + "\" ");
            b.append("status=\"" + chart.getEdgeStatus(id) + "\" ");
            b.append("caption=\"" + chart.getEdgeCaption(id) + "\" ");
            b.append("/>\n");
        }
        b.append("</kahinaChart>");
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
            m = new KahinaDbChart(db);
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
