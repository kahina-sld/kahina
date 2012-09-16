package org.kahina.lp.control;

import org.kahina.core.control.KahinaControlActuator;
import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaController;
import org.kahina.core.data.breakpoint.KahinaControlAgent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.lp.LogicProgrammingInstance;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.lp.bridge.LogicProgrammingBridge;

public class LogicProgrammingBreakActuator extends KahinaControlActuator
{
    public LogicProgrammingBreakActuator(KahinaController control)
    {
        super(control);
    }
    
    public void act(KahinaControlAgent agent)
    {
        control.processEvent(new LogicProgrammingAgentMatchEvent(agent, ControlAgentType.BREAK_AGENT));
    }
}
