package org.kahina.lp.control;

import org.kahina.core.control.KahinaControlActuator;
import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaController;
import org.kahina.core.data.agent.KahinaControlAgent;
import org.kahina.lp.LogicProgrammingInstance;

public class LogicProgrammingCompleteActuator extends KahinaControlActuator
{
    public LogicProgrammingCompleteActuator(LogicProgrammingInstance<?,?,?,?> kahina)
    {
        super(kahina);
    }
    
    public void act(KahinaControlAgent agent)
    {
        kahina.dispatchEvent(new LogicProgrammingAgentMatchEvent(agent, ControlAgentType.COMPLETE_AGENT));
    }
}
