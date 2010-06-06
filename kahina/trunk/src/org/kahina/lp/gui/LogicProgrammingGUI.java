package org.kahina.lp.gui;

import java.awt.Color;
import java.awt.event.KeyEvent;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaStep;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.lp.LogicProgrammingStepType;

public class LogicProgrammingGUI extends KahinaGUI
{
    public LogicProgrammingGUI(Class<? extends KahinaStep> stepType, KahinaInstance<?, ?, ?> kahina)
    {
        super(stepType, kahina);
        getControlPanel().addControlButtonGroup("Control");
        getControlPanel().addControlButton("creep.png", "creep", "(C)ontinue to next step", "Control", KeyEvent.VK_C);
        getControlPanel().addControlButton("roundskip.png", "auto-complete", "(A)uto-complete this step", "Control", KeyEvent.VK_A);
        getControlPanel().addControlButton("pause.png", "(un)pause", "(P)ause the current skip operation", "Control", KeyEvent.VK_P);
        getControlPanel().addControlButton("skip.png", "skip", "(S)kip this step", "Control", KeyEvent.VK_S);
        getControlPanel().addControlButton("reject.png", "fail", "make this step (F)ail", "Control", KeyEvent.VK_F);
        getControlPanel().addControlButton("leap.png", "leap", "(L)eap to next breakpoint match",  "Control", KeyEvent.VK_L);
        getControlPanel().addControlButton("stop.png", "stop", "abort skip or leap (X)",  "Control", KeyEvent.VK_X);
        
        getControlPanel().addControlButtonGroup("History");
        getControlPanel().addControlButton("back.png", "backInHistory", "Back (Q)",  "History", KeyEvent.VK_Q);
        getControlPanel().addControlButton("forward.png", "forwardInHistory", "Forward (W)",  "History", KeyEvent.VK_W);
        
        mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.CALL, Color.WHITE);
        mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.EXIT, new Color(153,255,102));
        mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.DET_EXIT, new Color(102,153,102));
        mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.FAIL, new Color(183,50,50));
        mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.REDO, new Color(204,102,0));
        mainTreeView.setStatusColorEncoding(LogicProgrammingStepType.PSEUDO_UNBLOCKED, Color.LIGHT_GRAY);
    }
}
