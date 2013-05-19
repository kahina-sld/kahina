package org.kahina.lp.data.agent;

import org.kahina.core.control.KahinaControlActuator;
import org.kahina.core.data.agent.KahinaControlAgent;
import org.kahina.core.data.agent.KahinaControlAgentProfile;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.lp.LogicProgrammingInstance;
import org.kahina.lp.LogicProgrammingState;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class LogicProgrammingControlAgentProfile extends KahinaControlAgentProfile
{
    
    public LogicProgrammingControlAgentProfile(KahinaControlActuator actuator)
    {
        super(actuator);
    }
    
    public static LogicProgrammingControlAgentProfile importXML(Element controlAgentElement, KahinaControlActuator actuator, LogicProgrammingInstance<?,?,?,?> kahina)
    {
        LogicProgrammingControlAgentProfile profile = new LogicProgrammingControlAgentProfile(actuator);
        NodeList controlAgentNodes = controlAgentElement.getElementsByTagName("kahina:controlAgent");
        for (int i = 0; i < controlAgentNodes.getLength(); i++)
        {
            Element controlAgentNode = (Element) controlAgentNodes.item(i);
            LogicProgrammingControlAgent controlAgent = LogicProgrammingControlAgent.importXML(controlAgentNode, kahina);
            profile.addControlAgent(controlAgent);
        }
        return profile;
    }
}
