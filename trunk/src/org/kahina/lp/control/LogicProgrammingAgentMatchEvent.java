package org.kahina.lp.control;

import org.kahina.core.control.KahinaEvent;
import org.kahina.core.data.breakpoint.KahinaControlPoint;

public class LogicProgrammingAgentMatchEvent extends KahinaEvent
{
    KahinaControlPoint agent;
    ControlAgentType type;
    
    public LogicProgrammingAgentMatchEvent(KahinaControlPoint agent, ControlAgentType type)
    {
        super("LP agent match");
        this.agent = agent;
        this.type = type;
    }
    
    public KahinaControlPoint getAgent()
    {
        return agent;
    }
    
    public ControlAgentType getAgentType()
    {
        return type;
    }

    public String toString()
    {
        return "LP agent match: " + agent.getName() + " of type " + type;
    }
}
