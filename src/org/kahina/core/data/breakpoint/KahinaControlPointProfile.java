package org.kahina.core.data.breakpoint;

import java.util.ArrayList;
import java.util.List;

import org.kahina.core.data.KahinaObject;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

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
        Element el = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:control-point-profile");
        for (KahinaControlPoint controlPoint : controlPoints)
        {
            el.appendChild(controlPoint.exportXML(dom));
        }
        return el;
    }
}
