package org.kahina.core.data.breakpoint;

import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.io.util.XMLUtil;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class KahinaControlPointProfile extends KahinaObject
{
    List<KahinaControlPoint> controlPoints;
    
    public KahinaControlPointProfile()
    {
        controlPoints = new ArrayList<KahinaControlPoint>();
    }
    
    public void addControlPoint(KahinaControlPoint controlPoint)
    {
        controlPoints.add(controlPoint);
    }
    
    public KahinaControlPoint getControlPoint(int index)
    {
        return controlPoints.get(index);
    }
    
    public KahinaControlPoint removeControlPoint(int index)
    {
        return controlPoints.remove(index);
    }
    
    public KahinaControlPoint[] getControlPoints()
    {
        return controlPoints.toArray(new KahinaControlPoint[controlPoints.size()]);
    }
    
    public int getSize()
    {
        return controlPoints.size();
    }
    
    public Element exportXML(Document dom)
    {
        Element el = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:controlPointProfile");
        for (KahinaControlPoint controlPoint : controlPoints)
        {
            el.appendChild(controlPoint.exportXML(dom));
        }
        return el;
    }
    
    public static KahinaControlPointProfile importXML(Element controlPointElement)
    {
        KahinaControlPointProfile profile = new KahinaControlPointProfile();
        NodeList controlPointNodes = controlPointElement.getElementsByTagName("kahina:controlPoint");
        for (int i = 0; i < controlPointNodes.getLength(); i++)
        {
            Element controlPointNode = (Element) controlPointNodes.item(i);
            KahinaControlPoint controlPoint = KahinaControlPoint.importXML(controlPointNode);
            profile.addControlPoint(controlPoint);
        }
        return profile;
    }
}
