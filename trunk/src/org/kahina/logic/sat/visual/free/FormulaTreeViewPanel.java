package org.kahina.logic.sat.visual.free;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.tree.KahinaTreeView;
import org.kahina.core.visual.tree.KahinaTreeViewListener;
import org.kahina.core.visual.tree.KahinaTreeViewPanel;

public class FormulaTreeViewPanel extends KahinaTreeViewPanel
{
    FormulaTreeView view;
    
    public FormulaTreeViewPanel(KahinaInstance<?, ?, ?> kahina)
    {
        super(kahina);
        this.removeMouseListener(this.getMouseListeners()[0]);
        this.addMouseListener(new FormulaTreeViewListener(this, kahina));
    }
    
    public void setView(KahinaTreeView view)
    {
        super.setView(view);
        if (view instanceof FormulaTreeView)
        {
            this.view = (FormulaTreeView) view;
        }          
    }
}
