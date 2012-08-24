package org.kahina.lp.control;

import org.kahina.core.control.KahinaControlActuator;
import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaController;

public class LogicProgrammingSkipActuator extends KahinaControlActuator
{
    public LogicProgrammingSkipActuator(KahinaController control)
    {
        super(control);
    }
    
    public void act()
    {
        control.processEvent(new KahinaControlEvent("skip"));
    }    
}
