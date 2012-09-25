package org.kahina.lp.control;

import org.kahina.core.control.KahinaControlActuator;
import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaController;
import org.kahina.core.data.agent.KahinaControlAgent;

public class LogicProgrammingSkipActuator extends KahinaControlActuator
{
    public LogicProgrammingSkipActuator(KahinaController control)
    {
        super(control);
    }
    
    public void act(KahinaControlAgent agent)
    {
        control.processEvent(new LogicProgrammingAgentMatchEvent(agent, ControlAgentType.SKIP_AGENT));
    }    
}
