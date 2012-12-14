package org.kahina.logic.sat.muc.visual;

import javax.swing.JComponent;

import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.visual.cnf.list.KahinaSatInstanceListView;

public class MetaInstanceView extends KahinaSatInstanceListView
{
    MUCInstance kahina;
    
    public MetaInstanceView(MUCInstance kahina)
    {
        super(kahina);
        this.kahina = kahina;
    }
    
    @Override
    public JComponent makePanel()
    {
        MetaInstanceViewPanel panel = new MetaInstanceViewPanel(kahina);
        kahina.registerInstanceListener("redraw", panel);
        panel.setView(this);
        return panel;
    }
}
