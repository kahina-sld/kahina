package org.kahina.lp.control;

import org.kahina.core.control.KahinaControlActuator;
import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaController;

public class LogicProgrammingFailActuator extends KahinaControlActuator
{
    public LogicProgrammingFailActuator(KahinaController control)
    {
        super(control);
    }
    
    public void act()
    {
        control.processEvent(new KahinaControlEvent("fail"));
    }
}
