package org.kahina.logic.sat.muc.visual;

import javax.swing.JComponent;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.tree.KahinaTreeView;

public class RecursiveBlockView  extends KahinaTreeView
{

    public RecursiveBlockView(KahinaInstance<?, ?, ?, ?> kahina)
    {
        super(kahina);
        // TODO Auto-generated constructor stub
    }

    @Override
    public JComponent makePanel()
    {
        RecursiveBlockViewPanel panel = new RecursiveBlockViewPanel(kahina);
        kahina.registerInstanceListener("redraw", panel);
        panel.setView(this);
        return panel;
    }

}
