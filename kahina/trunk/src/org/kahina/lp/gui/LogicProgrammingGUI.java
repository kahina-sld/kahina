package org.kahina.lp.gui;

import java.awt.Color;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaStep;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.lp.LogicProgrammingStepType;

public class LogicProgrammingGUI extends KahinaGUI
{
    public LogicProgrammingGUI(Class<? extends KahinaStep> stepType, KahinaInstance<?, ?, ?> kahina)
    {
        super(stepType, kahina);
        getControlPanel().addControlButton("creep.png", "creep", "continue to next step");
        getControlPanel().addControlButton("reject.png", "fail", "make this step fail");
        getControlPanel().addControlButton("roundskip.png", "skip", "auto-complete this step");
        getControlPanel().addControlButton("pause.png", "(un)pause", "pause the current skip operation");
        getControlPanel().addControlButton("leap.png", "leap", "jump to next breakpoint match");
        getControlPanel().addControlButton("stop.png", "stop", "abort skip or leap");
        
        mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.CALL, Color.WHITE);
        mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.EXIT, new Color(153,255,102));
        mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.DET_EXIT, new Color(102,153,102));
        mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.FAIL, new Color(183,50,50));
        mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.REDO, new Color(204,102,0));
    }
}
