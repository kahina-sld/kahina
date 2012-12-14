package org.kahina.logic.sat.muc.visual;

import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.visual.cnf.list.KahinaSatInstanceListViewPanel;

public class MetaInstanceViewPanel extends KahinaSatInstanceListViewPanel
{
    MUCInstance kahina;
    
    public MetaInstanceViewPanel(MUCInstance kahina)
    {
        super();
        this.kahina = kahina;
        getList().addMouseListener(new MetaInstanceViewListener(kahina, this));
    }
}
