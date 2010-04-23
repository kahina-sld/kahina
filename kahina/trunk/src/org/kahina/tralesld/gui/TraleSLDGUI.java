package org.kahina.tralesld.gui;

import java.awt.Color;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaStep;
import org.kahina.core.control.KahinaController;
import org.kahina.lp.LogicProgrammingStepType;
import org.kahina.lp.gui.LogicProgrammingGUI;
import org.kahina.tralesld.TraleSLDStepType;

public class TraleSLDGUI extends LogicProgrammingGUI
{
    public TraleSLDGUI(Class<? extends KahinaStep> stepType, KahinaInstance kahina, KahinaController control)
    {
        super(stepType, kahina, control);
        
        mainTreeView.setStatusColorEncoding(TraleSLDStepType.FINISHED, new Color(102,51,153));
        mainTreeView.setStatusColorEncoding(TraleSLDStepType.BLOCKED, Color.BLACK);
        //TODO: build font color customization facilities into TreeView
        //mainTreeView.setStatusFontColorEncoding(TraleSLDStepType.BLOCKED, Color.BLACK);
    }
}
