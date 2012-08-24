package org.kahina.lp.control;

import org.kahina.core.control.KahinaControlActuator;
import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaController;

public class LogicProgrammingCompleteActuator extends KahinaControlActuator
{
    public LogicProgrammingCompleteActuator(KahinaController control)
    {
        super(control);
    }
    
    public void act()
    {
        control.processEvent(new KahinaControlEvent("auto-complete"));
    }
}
