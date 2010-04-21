package org.kahina.lp.gui;

import java.awt.Color;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaStep;
import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.lp.LogicProgrammingStepType;

public class LogicProgrammingGUI extends KahinaGUI
{
    public LogicProgrammingGUI(Class<? extends KahinaStep> stepType, KahinaInstance kahina, KahinaController control)
    {
        super(stepType, kahina, control);
        getControlPanel().addControlButton("creep.png", "creep", "continue to next step");
        getControlPanel().addControlButton("roundskip.png", "skip", "auto-complete this step");
        getControlPanel().addControlButton("reject.png", "fail", "make this step fail");
        getControlPanel().addControlButton("leap.png", "leap", "jump to next breakpoint match");
        getControlPanel().addControlButton("pause.png", "(un)pause", "pause the current skip operation");
        
        mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.CALL, Color.WHITE);
        mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.EXIT, Color.GREEN);
        mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.DET_EXIT, Color.GRAY);
        mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.FAIL, Color.RED);
        mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.REDO, Color.ORANGE);
    }
}
