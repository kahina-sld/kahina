package org.kahina.lp.data.project;

import java.io.File;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.breakpoint.KahinaControlPoint;
import org.kahina.core.data.breakpoint.KahinaControlPointProfile;
import org.kahina.core.data.project.KahinaProject;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.lp.control.LogicProgrammingBreakActuator;
import org.kahina.lp.control.LogicProgrammingCompleteActuator;
import org.kahina.lp.control.LogicProgrammingCreepActuator;
import org.kahina.lp.control.LogicProgrammingFailActuator;
import org.kahina.lp.control.LogicProgrammingSkipActuator;
import org.kahina.lp.control.NewControlAgentEvent;
import org.kahina.lp.data.breakpoint.LogicProgrammingControlPointProfile;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class LogicProgrammingProject extends KahinaProject
{
    //store profiles for the various types of  control points
    protected KahinaControlPointProfile breakPoints;
    protected KahinaControlPointProfile creepPoints;
    protected KahinaControlPointProfile completePoints;
    protected KahinaControlPointProfile skipPoints;
    protected KahinaControlPointProfile failPoints;
    protected KahinaControlPointProfile warnPoints;
    
    public LogicProgrammingProject(String appID, KahinaTree stepTree, KahinaController control)
    {
        super(appID);
        breakPoints = new LogicProgrammingControlPointProfile(new LogicProgrammingBreakActuator(control), stepTree);
        creepPoints = new LogicProgrammingControlPointProfile(new LogicProgrammingCreepActuator(control), stepTree);
        completePoints = new LogicProgrammingControlPointProfile(new LogicProgrammingCompleteActuator(control), stepTree);
        skipPoints = new LogicProgrammingControlPointProfile(new LogicProgrammingSkipActuator(control), stepTree);
        failPoints = new LogicProgrammingControlPointProfile(new LogicProgrammingFailActuator(control), stepTree);
        //warnPoints = new KahinaControlPointProfile();
    }
    
    public KahinaControlPointProfile getBreakPoints()
    {
        return breakPoints;
    }
    
    public KahinaControlPointProfile getCreepPoints()
    {
        return creepPoints;
    }
    
    public KahinaControlPointProfile getCompletePoints()
    {
        return completePoints;
    }
    
    public KahinaControlPointProfile getSkipPoints()
    {
        return skipPoints;
    }
    
    public KahinaControlPointProfile getFailPoints()
    {
        return failPoints;
    }
    
    public KahinaControlPointProfile getWarnPoints()
    {
        return warnPoints;
    }
    
    public void addBreakPoint(KahinaControlPoint agent)
    {
        breakPoints.addControlPoint(agent);
    }
    
    public void addCreepPoint(KahinaControlPoint agent)
    {
        creepPoints.addControlPoint(agent);
    }
    
    public void addCompletePoint(KahinaControlPoint agent)
    {
        completePoints.addControlPoint(agent);
    }
    
    public void addSkipPoint(KahinaControlPoint agent)
    {
        skipPoints.addControlPoint(agent);
    }
    
    public void addFailPoint(KahinaControlPoint agent)
    {
        failPoints.addControlPoint(agent);
    }
    
    public void addWarnPoint(KahinaControlPoint agent)
    {
        warnPoints.addControlPoint(agent);
    }
    
    public static LogicProgrammingProject importXML(Element topEl, LogicProgrammingProject project)
    {
        KahinaProject.importXML(topEl, project);
        //TODO: read in control agent profiles
        return project;
    }
}
