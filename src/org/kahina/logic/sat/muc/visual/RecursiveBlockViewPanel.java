package org.kahina.logic.sat.muc.visual;

import java.awt.image.BufferedImage;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.tree.KahinaTreeView;
import org.kahina.core.visual.tree.KahinaTreeViewListener;
import org.kahina.core.visual.tree.KahinaTreeViewPanel;
import org.kahina.logic.sat.muc.MUCInstance;

public class RecursiveBlockViewPanel extends KahinaTreeViewPanel
{
    RecursiveBlockView view;
    
    public RecursiveBlockViewPanel(MUCInstance kahina)
    {
        super(kahina);
        this.removeMouseListener(this.getMouseListeners()[0]);
        this.addMouseListener(new RecursiveBlockViewListener(this, kahina));
    }
    
    public void setView(RecursiveBlockView view)
    {
        this.view = view;
        super.setView(view);
    }
}
