package org.kahina.lp.data.agent;

import org.kahina.core.control.KahinaControlActuator;
import org.kahina.core.data.agent.KahinaControlAgent;
import org.kahina.core.data.agent.KahinaControlAgentProfile;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.lp.LogicProgrammingState;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class LogicProgrammingControlAgentProfile extends KahinaControlAgentProfile
{
    protected KahinaTree stepTree;
    
    public LogicProgrammingControlAgentProfile(KahinaControlActuator actuator, KahinaTree stepTree)
    {
        super(actuator);
        this.stepTree = stepTree;
    }
    
    public void addControlAgent(LogicProgrammingControlAgent controlAgent)
    {
        super.addControlAgent(controlAgent);
        controlAgent.setStepTree(stepTree);
    }
    
    public static LogicProgrammingControlAgentProfile importXML(Element controlAgentElement, KahinaControlActuator actuator, KahinaTree stepTree)
    {
        LogicProgrammingControlAgentProfile profile = new LogicProgrammingControlAgentProfile(actuator, stepTree);
        NodeList controlAgentNodes = controlAgentElement.getElementsByTagName("kahina:controlAgent");
        for (int i = 0; i < controlAgentNodes.getLength(); i++)
        {
            Element controlAgentNode = (Element) controlAgentNodes.item(i);
            LogicProgrammingControlAgent controlAgent = LogicProgrammingControlAgent.importXML(controlAgentNode, actuator.getKahina(), stepTree);
            profile.addControlAgent(controlAgent);
        }
        return profile;
    }
}
