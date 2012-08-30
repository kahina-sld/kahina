package org.kahina.lp.control;

import org.kahina.core.control.KahinaEvent;
import org.kahina.core.data.breakpoint.KahinaControlPoint;

public class NewControlAgentEvent extends KahinaEvent
{
    KahinaControlPoint controlAgent;
    ControlAgentType agentType;
    
    public NewControlAgentEvent(KahinaControlPoint controlAgent, ControlAgentType agentType)
    {
        super("new agent");
        this.controlAgent = controlAgent;
        this.agentType = agentType;
    }  
    
    /**
     * Returns the control agent transported by this event, to which an actuator needs to be added according to the agent type.
     * @return the control agent transported by this event.
     */
    public KahinaControlPoint getControlAgent()
    {
        return controlAgent;
    }
    
    public ControlAgentType getAgentType()
    {
        return agentType;
    }
}
