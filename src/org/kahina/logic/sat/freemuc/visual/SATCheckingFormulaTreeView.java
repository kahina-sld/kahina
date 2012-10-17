package org.kahina.logic.sat.freemuc.visual;

import javax.swing.JComponent;

import org.kahina.logic.sat.freemuc.FreeMUCInstance;
import org.kahina.logic.sat.visual.free.FormulaTreeView;
import org.kahina.logic.sat.visual.free.FormulaTreeViewPanel;

public class SATCheckingFormulaTreeView extends FormulaTreeView
{
    FreeMUCInstance kahina;
    
    public SATCheckingFormulaTreeView(FreeMUCInstance kahina)
    {
        super(kahina);
        this.kahina = kahina;
    }

    @Override
    public JComponent makePanel()
    {
        FormulaTreeViewPanel panel = new FormulaTreeViewPanel(kahina);
        panel.removeMouseListener(panel.getMouseListeners()[0]);
        panel.addMouseListener(new SATCheckingFormulaTreeListener(panel, kahina));
        kahina.registerInstanceListener("redraw", panel);
        panel.setView(this);
        return panel;
    }
}
