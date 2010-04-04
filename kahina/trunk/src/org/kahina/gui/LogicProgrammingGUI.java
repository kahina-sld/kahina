package org.kahina.gui;

import org.kahina.control.KahinaController;
import org.kahina.core.KahinaInstance;

public class LogicProgrammingGUI extends KahinaGUI
{
    public LogicProgrammingGUI(KahinaInstance kahina, KahinaController control)
    {
        super(kahina, control);
        getControlPanel().addControlButton("creep.png", "creep", "continue to next step");
        getControlPanel().addControlButton("roundskip.png", "skip", "auto-complete this step");
        getControlPanel().addControlButton("reject.png", "fail", "make this step fail");
        getControlPanel().addControlButton("leap.png", "leap", "jump to next breakpoint match");
    }
}
