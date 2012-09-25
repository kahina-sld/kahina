package org.kahina.lp.control;

import org.kahina.core.control.KahinaControlActuator;
import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaController;
import org.kahina.core.data.agent.KahinaControlAgent;

public class LogicProgrammingCreepActuator extends KahinaControlActuator
{
    public LogicProgrammingCreepActuator(KahinaController control)
    {
        super(control);
    }
    
    public void act(KahinaControlAgent agent)
    {
        control.processEvent(new LogicProgrammingAgentMatchEvent(agent, ControlAgentType.CREEP_AGENT));
    } 
}
