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

public class KahinaControlAgentProfile extends KahinaObject
{
    List<KahinaControlAgent> controlAgents;
    KahinaControlActuator actuator;
    
    /**
     * Builds a profile consisting of control points with a common actuator.
     * @param actuator the common actuator for all the control points in the profile
     */
    public KahinaControlAgentProfile(KahinaControlActuator actuator)
    {
        this.actuator = actuator;
        controlAgents = new ArrayList<KahinaControlAgent>();
    }
    
    public KahinaControlAgentProfile copy()
    {
        KahinaControlAgentProfile copy = new KahinaControlAgentProfile(actuator);
        for (KahinaControlAgent agent : controlAgents)
        {
            copy.controlAgents.add(agent.copy());
        }
        return copy;
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
    
    public void addControlAgent(KahinaControlAgent controlAgent)
    {
        controlAgents.add(controlAgent);
        controlAgent.setActuator(actuator);
    }
    
    public KahinaControlAgent getControlAgent(int index)
    {
        return controlAgents.get(index);
    }
    
    public KahinaControlAgent removeControlAgent(int index)
    {
        return controlAgents.remove(index);
    }
    
    public KahinaControlAgent[] getControlPoints()
    {
        return controlAgents.toArray(new KahinaControlAgent[controlAgents.size()]);
    }
    
    public int getSize()
    {
        return controlAgents.size();
    }
    
    public Element exportXML(Document dom)
    {
        Element el = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:controlAgentProfile");
        for (KahinaControlAgent controlAgent : controlAgents)
        {
            el.appendChild(controlAgent.exportXML(dom));
        }
        return el;
    }
    
    public static KahinaControlAgentProfile importXML(Element controlAgentElement, KahinaControlActuator actuator)
    {
        //default actuator: LogicProgrammingBreakActuator; can be changed later
        KahinaControlAgentProfile profile = new KahinaControlAgentProfile(actuator);
        NodeList controlAgentNodes = controlAgentElement.getElementsByTagName("kahina:controlAgent");
        for (int i = 0; i < controlAgentNodes.getLength(); i++)
        {
            Element controlPointNode = (Element) controlAgentNodes.item(i);
            KahinaControlAgent controlAgent = KahinaControlAgent.importXML(controlPointNode, actuator.getControl());
            profile.addControlAgent(controlAgent);
        }
        return profile;
    }
}
