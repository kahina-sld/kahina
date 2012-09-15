package org.kahina.lp.data.breakpoint;

import org.kahina.core.control.KahinaControlActuator;
import org.kahina.core.data.breakpoint.KahinaControlPoint;
import org.kahina.core.data.breakpoint.KahinaControlPointProfile;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.lp.LogicProgrammingState;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class LogicProgrammingControlPointProfile extends KahinaControlPointProfile
{
    protected KahinaTree stepTree;
    
    public LogicProgrammingControlPointProfile(KahinaControlActuator actuator, KahinaTree stepTree)
    {
        super(actuator);
        this.stepTree = stepTree;
    }
    
    public void addControlPoint(LogicProgrammingControlPoint controlPoint)
    {
        super.addControlPoint(controlPoint);
        controlPoint.setStepTree(stepTree);
    }
    
    public static LogicProgrammingControlPointProfile importXML(Element controlPointElement, KahinaControlActuator actuator, KahinaTree stepTree)
    {
        LogicProgrammingControlPointProfile profile = new LogicProgrammingControlPointProfile(actuator, stepTree);
        NodeList controlPointNodes = controlPointElement.getElementsByTagName("kahina:controlPoint");
        for (int i = 0; i < controlPointNodes.getLength(); i++)
        {
            Element controlPointNode = (Element) controlPointNodes.item(i);
            LogicProgrammingControlPoint controlPoint = LogicProgrammingControlPoint.importXML(controlPointNode, actuator.getControl(), stepTree);
            profile.addControlPoint(controlPoint);
        }
        return profile;
    }
}
