package org.kahina.logic.sat.visual.free;

import java.awt.event.MouseEvent;

import org.kahina.core.KahinaInstance;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.visual.tree.KahinaTreeViewListener;
import org.kahina.core.visual.tree.KahinaTreeViewOptions;
import org.kahina.core.visual.tree.KahinaTreeViewPanel;

public class FormulaTreeViewListener extends KahinaTreeViewListener
{
    protected FormulaTreeViewPanel view;
    
    public FormulaTreeViewListener(FormulaTreeViewPanel view, KahinaInstance<?, ?, ?, ?> kahina)
    {
        super(view,kahina);
        this.view = view;
    }
    
    @Override
    public void mouseClicked(MouseEvent e)
    {
        int clickedNode = view.view.nodeAtCoordinates(e.getX(), e.getY());
        //view.view.recursiveDecollapse(clickedNode);
        view.view.cycleDecollapsePrune(clickedNode);
        view.view.recalculate();
        view.updateDisplayAndRepaintFromEventDispatchThread();
    }
}
