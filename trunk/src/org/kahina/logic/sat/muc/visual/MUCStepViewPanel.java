package org.kahina.logic.sat.muc.visual;

import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.visual.cnf.list.KahinaSatInstanceListViewPanel;

public class MUCStepViewPanel extends KahinaSatInstanceListViewPanel
{
    MUCInstance kahina;
    MUCStepView view;
    
    public MUCStepViewPanel(MUCInstance kahina)
    {
        super(kahina);
        this.kahina = kahina;
        list.addMouseListener(new MUCStepViewListener(kahina, this));
    }
    
    public void setView(MUCStepView view)
    {
        super.setView(view);
        this.view = view;
    }
}
