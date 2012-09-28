package org.kahina.lp.control;

import org.kahina.core.control.KahinaControlActuator;
import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaController;
import org.kahina.core.data.agent.KahinaControlAgent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.lp.LogicProgrammingInstance;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.lp.bridge.LogicProgrammingBridge;

public class LogicProgrammingBreakActuator extends KahinaControlActuator
{
    public LogicProgrammingBreakActuator(LogicProgrammingInstance<?,?,?,?> kahina)
    {
        super(kahina);
    }
    
    public void act(KahinaControlAgent agent)
    {
        kahina.dispatchEvent(new LogicProgrammingAgentMatchEvent(agent, ControlAgentType.BREAK_AGENT));
    }
}
