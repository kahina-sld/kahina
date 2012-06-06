package org.kahina.logic.sat.visual.free;

import java.awt.event.MouseEvent;

import org.kahina.core.KahinaInstance;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.visual.tree.KahinaTreeViewListener;
import org.kahina.core.visual.tree.KahinaTreeViewOptions;
import org.kahina.core.visual.tree.KahinaTreeViewPanel;

public class FormulaTreeViewListener extends KahinaTreeViewListener
{
    FormulaTreeViewPanel view;
    
    public FormulaTreeViewListener(FormulaTreeViewPanel view, KahinaInstance<?, ?, ?> kahina)
    {
        super(view,kahina);
        this.view = view;
    }
    
    @Override
    public void mouseClicked(MouseEvent e)
    {
        int clickedNode = view.view.nodeAtCoordinates(e.getX(), e.getY());
        if (lastMouseEvent != null && e.getWhen() - lastMouseEvent.getWhen() < 500)
        {
            view.view.recursiveDecollapse(clickedNode);
            //view.view.toggleFormulaCollapse(clickedNode);
            view.view.recalculate();
            view.updateDisplayAndRepaintFromEventDispatchThread();
        }
        else
        {
            view.view.toggleFormulaCollapse(clickedNode);
            view.view.recalculate();
            view.updateDisplayAndRepaintFromEventDispatchThread();
            //kahina.dispatchEvent(new KahinaSelectionEvent(clickedNode));
            lastMouseEvent = e;
        }
    }
}
