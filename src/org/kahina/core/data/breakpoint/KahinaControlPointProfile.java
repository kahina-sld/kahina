package org.kahina.core.data.breakpoint;

import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import org.kahina.core.control.KahinaControlActuator;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.io.util.XMLUtil;
import org.kahina.lp.control.LogicProgrammingBreakActuator;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class KahinaControlPointProfile extends KahinaObject
{
    List<KahinaControlPoint> controlPoints;
    KahinaControlActuator actuator;
    
    /**
     * Builds a profile consisting of control points with a common actuator.
     * @param actuator the common actuator for all the control points in the profile
     */
    public KahinaControlPointProfile(KahinaControlActuator actuator)
    {
        this.actuator = actuator;
        controlPoints = new ArrayList<KahinaControlPoint>();
    }
    
    public KahinaControlActuator getActuator()
    {
        return actuator;
    }
    
    //not necessary for now, could even be a misleading option
    /*public void setCommonActuator(KahinaControlActuator actuator)
    {
        this.actuator = actuator;
        for (KahinaControlPoint controlPoint : controlPoints)
        {
            controlPoint.setActuator(actuator);
        }
    }*/
    
    public void addControlPoint(KahinaControlPoint controlPoint)
    {
        controlPoints.add(controlPoint);
        controlPoint.setActuator(actuator);
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
    
    public static KahinaControlPointProfile importXML(Element controlPointElement, KahinaControlActuator actuator)
    {
        //default actuator: LogicProgrammingBreakActuator; can be changed later
        KahinaControlPointProfile profile = new KahinaControlPointProfile(actuator);
        NodeList controlPointNodes = controlPointElement.getElementsByTagName("kahina:controlPoint");
        for (int i = 0; i < controlPointNodes.getLength(); i++)
        {
            Element controlPointNode = (Element) controlPointNodes.item(i);
            KahinaControlPoint controlPoint = KahinaControlPoint.importXML(controlPointNode, actuator.getControl());
            profile.addControlPoint(controlPoint);
        }
        return profile;
    }
}
