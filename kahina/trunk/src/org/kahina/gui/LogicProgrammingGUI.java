package org.kahina.gui;

import org.kahina.control.KahinaController;
import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaStep;
import org.kahina.core.LogicProgrammingStep;

public class LogicProgrammingGUI extends KahinaGUI
{
    public LogicProgrammingGUI(Class<? extends KahinaStep> stepType, KahinaInstance kahina, KahinaController control)
    {
        super(stepType, kahina, control);
        getControlPanel().addControlButton("creep.png", "creep", "continue to next step");
        getControlPanel().addControlButton("roundskip.png", "skip", "auto-complete this step");
        getControlPanel().addControlButton("reject.png", "fail", "make this step fail");
        getControlPanel().addControlButton("leap.png", "leap", "jump to next breakpoint match");
    }
}
