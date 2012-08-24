package org.kahina.lp.control;

import org.kahina.core.control.KahinaControlActuator;
import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaController;

public class LogicProgrammingBreakActuator extends KahinaControlActuator
{
    public LogicProgrammingBreakActuator(KahinaController control)
    {
        super(control);
    }
    
    public void act()
    {
        control.processEvent(new KahinaControlEvent("(un)pause"));
    }
}
