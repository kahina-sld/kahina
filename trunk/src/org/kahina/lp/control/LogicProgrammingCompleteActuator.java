package org.kahina.lp.control;

import org.kahina.core.control.KahinaControlActuator;
import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaController;
import org.kahina.core.data.agent.KahinaControlAgent;

public class LogicProgrammingCompleteActuator extends KahinaControlActuator
{
    public LogicProgrammingCompleteActuator(KahinaController control)
    {
        super(control);
    }
    
    public void act(KahinaControlAgent agent)
    {
        control.processEvent(new LogicProgrammingAgentMatchEvent(agent, ControlAgentType.COMPLETE_AGENT));
    }
}
