package org.kahina.lp.data.project;

import java.io.File;

import org.kahina.core.control.KahinaControlActuator;
import org.kahina.core.control.KahinaController;
import org.kahina.core.data.agent.KahinaControlAgent;
import org.kahina.core.data.agent.KahinaControlAgentProfile;
import org.kahina.core.data.project.KahinaProject;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.lp.control.LogicProgrammingBreakActuator;
import org.kahina.lp.control.LogicProgrammingCompleteActuator;
import org.kahina.lp.control.LogicProgrammingCreepActuator;
import org.kahina.lp.control.LogicProgrammingFailActuator;
import org.kahina.lp.control.LogicProgrammingSkipActuator;
import org.kahina.lp.control.NewControlAgentEvent;
import org.kahina.lp.data.agent.LogicProgrammingControlAgentProfile;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class LogicProgrammingProject extends KahinaProject
{
    //store profiles for the various types of  control points
    protected KahinaControlAgentProfile breakPoints;
    protected KahinaControlAgentProfile creepPoints;
    protected KahinaControlAgentProfile completePoints;
    protected KahinaControlAgentProfile skipPoints;
    protected KahinaControlAgentProfile failPoints;
    protected KahinaControlAgentProfile warnPoints;
    
    protected KahinaTree stepTree;
    protected KahinaController control;
    
    public LogicProgrammingProject(String appID, String name, KahinaTree stepTree, KahinaController control)
    {
        super(appID, name);
        this.stepTree = stepTree;
        this.control = control;
        breakPoints = new LogicProgrammingControlAgentProfile(new LogicProgrammingBreakActuator(control), stepTree);
        creepPoints = new LogicProgrammingControlAgentProfile(new LogicProgrammingCreepActuator(control), stepTree);
        completePoints = new LogicProgrammingControlAgentProfile(new LogicProgrammingCompleteActuator(control), stepTree);
        skipPoints = new LogicProgrammingControlAgentProfile(new LogicProgrammingSkipActuator(control), stepTree);
        failPoints = new LogicProgrammingControlAgentProfile(new LogicProgrammingFailActuator(control), stepTree);
        //warnPoints = new KahinaControlPointProfile();
    }
    
    public LogicProgrammingProject copy()
    {
        LogicProgrammingProject copy = new LogicProgrammingProject(appID, new String(name), stepTree, control);
        copyDataInto(copy);
        return copy;
    }
   
    public void copyDataInto(LogicProgrammingProject copy)
    {
        super.copyDataInto(copy);
        copy.breakPoints = breakPoints.copy();
        copy.creepPoints = creepPoints.copy();
        copy.completePoints = completePoints.copy();
        copy.skipPoints = skipPoints.copy();
        copy.failPoints = failPoints.copy();
    }
    
    public KahinaControlAgentProfile getBreakPoints()
    {
        return breakPoints;
    }
    
    public KahinaControlAgentProfile getCreepPoints()
    {
        return creepPoints;
    }
    
    public KahinaControlAgentProfile getCompletePoints()
    {
        return completePoints;
    }
    
    public KahinaControlAgentProfile getSkipPoints()
    {
        return skipPoints;
    }
    
    public KahinaControlAgentProfile getFailPoints()
    {
        return failPoints;
    }
    
    public KahinaControlAgentProfile getWarnPoints()
    {
        return warnPoints;
    }
    
    public void addBreakPoint(KahinaControlAgent agent)
    {
        breakPoints.addControlAgent(agent);
    }
    
    public void addCreepPoint(KahinaControlAgent agent)
    {
        creepPoints.addControlAgent(agent);
    }
    
    public void addCompletePoint(KahinaControlAgent agent)
    {
        completePoints.addControlAgent(agent);
    }
    
    public void addSkipPoint(KahinaControlAgent agent)
    {
        skipPoints.addControlAgent(agent);
    }
    
    public void addFailPoint(KahinaControlAgent agent)
    {
        failPoints.addControlAgent(agent);
    }
    
    public void addWarnPoint(KahinaControlAgent agent)
    {
        warnPoints.addControlAgent(agent);
    }
    
    public Element exportXML(Document dom)
    {
        Element el = super.exportXML(dom);
        Element profilesEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:controlAgentProfiles");
        Element profileEl = breakPoints.exportXML(dom);
        profileEl.setAttribute("kahina:type", "break");
        profilesEl.appendChild(profileEl);
        profileEl = creepPoints.exportXML(dom);
        profileEl.setAttribute("kahina:type", "creep");
        profilesEl.appendChild(profileEl);
        profileEl = completePoints.exportXML(dom);
        profileEl.setAttribute("kahina:type", "complete");
        profilesEl.appendChild(profileEl);
        profileEl = skipPoints.exportXML(dom);
        profileEl.setAttribute("kahina:type", "skip");
        profilesEl.appendChild(profileEl);
        profileEl = failPoints.exportXML(dom);
        profileEl.setAttribute("kahina:type", "fail");
        profilesEl.appendChild(profileEl);
        /*profileEl = warnPoints.exportXML(dom);
        profileEl.setAttribute("kahina:type", "warn");
        profilesEl.appendChild(profileEl);*/
        el.appendChild(profilesEl);
        return el;
    }
    
    public static LogicProgrammingProject importXML(Element topEl, LogicProgrammingProject project, KahinaController control, KahinaTree stepTree)
    {
        KahinaProject.importXML(topEl, project);
        //read in control agent profiles
        NodeList profileList = topEl.getElementsByTagName("kahina:controlAgentProfile");
        for (int i = 0; i < profileList.getLength(); i++)
        {
            Element profileElement = (Element) profileList.item(i);
            String type = profileElement.getAttribute("kahina:type");
            if (type.equals("break"))
            {
                KahinaControlActuator actuator = new LogicProgrammingBreakActuator(control);  
                project.breakPoints = LogicProgrammingControlAgentProfile.importXML(profileElement, actuator, stepTree);
            }
            else if (type.equals("creep"))
            {
                KahinaControlActuator actuator = new LogicProgrammingCreepActuator(control);  
                project.creepPoints = LogicProgrammingControlAgentProfile.importXML(profileElement, actuator, stepTree);
            }
            else if (type.equals("complete"))
            {
                KahinaControlActuator actuator = new LogicProgrammingCompleteActuator(control);  
                project.completePoints = LogicProgrammingControlAgentProfile.importXML(profileElement, actuator, stepTree);
            }
            else if (type.equals("skip"))
            {
                KahinaControlActuator actuator = new LogicProgrammingSkipActuator(control);  
                project.skipPoints = LogicProgrammingControlAgentProfile.importXML(profileElement, actuator, stepTree);
            }
            else if (type.equals("fail"))
            {
                KahinaControlActuator actuator = new LogicProgrammingFailActuator(control);  
                project.failPoints = LogicProgrammingControlAgentProfile.importXML(profileElement, actuator, stepTree);
            }
        }
        return project;
    }
}
