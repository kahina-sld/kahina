package org.kahina.logic.sat.muc.visual;

import java.awt.event.ActionListener;

import javax.swing.JPopupMenu;

import org.kahina.core.visual.graph.KahinaGraphView;
import org.kahina.logic.sat.muc.MUCInstance;

public class MUCStepViewContextMenu extends JPopupMenu
{
    MUCStepView view;
    MUCInstance kahina;
    
    public MUCStepViewContextMenu(MUCStepView view, MUCInstance kahina)
    {
        this.view = view;
        this.kahina = kahina;
    }
}
