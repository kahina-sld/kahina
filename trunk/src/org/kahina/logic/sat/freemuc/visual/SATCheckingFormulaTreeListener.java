package org.kahina.logic.sat.freemuc.visual;

import java.awt.event.MouseEvent;

import org.kahina.logic.sat.data.free.BooleanFormula;
import org.kahina.logic.sat.freemuc.FreeMUCInstance;
import org.kahina.logic.sat.visual.free.FormulaTreeViewListener;
import org.kahina.logic.sat.visual.free.FormulaTreeViewPanel;

public class SATCheckingFormulaTreeListener extends FormulaTreeViewListener
{
    FreeMUCInstance kahina;
    
    public SATCheckingFormulaTreeListener(FormulaTreeViewPanel view, FreeMUCInstance kahina)
    {
        super(view, kahina);
        this.kahina = kahina;
    }

    @Override
    public void mouseClicked(MouseEvent e)
    {
        int clickedNode = view.view.nodeAtCoordinates(e.getX(), e.getY());
        int oldStatus = view.view.getModel().getNodeStatus(clickedNode);
        if (e.isControlDown())
        {
            BooleanFormula frm = view.view.nodeToFrm.get(clickedNode);
            if (frm != null)
            {
                frm.setPruned(true);
                boolean pruningSuccess = true;
                pruningSuccess = view.view.isConjunct(clickedNode);
                System.err.println("isConjunct = " + pruningSuccess);
                if (pruningSuccess)
                {
                    pruningSuccess = !kahina.currentPruningSatisfiable();
                    System.err.println("staysUnsatisfiable = " + pruningSuccess);
                }
                System.err.println();
                if (!pruningSuccess)
                {
                    frm.setPruned(false);
                    if (oldStatus == 0)
                    {
                        view.view.getModel().setNodeStatus(clickedNode, 3);
                    }
                    else if (oldStatus == 1)
                    {
                        view.view.getModel().setNodeStatus(clickedNode, 4);
                    }
                }
                else
                {
                    if (oldStatus == 1)
                    {
                        view.view.formulaCollapse(clickedNode);
                    }
                    view.view.getModel().setNodeStatus(clickedNode, 2);
                }
            }
        }
        else
        {
            switch (oldStatus)
            {
                case 0:
                    if (view.view.isConjunction(clickedNode))
                    {
                        view.view.formulaDecollapse(clickedNode);
                    }
                    else
                    {
                        view.view.getModel().setNodeStatus(clickedNode, 1);
                    }
                    break;
                case 1:
                    if (view.view.isConjunction(clickedNode))
                    {
                        view.view.formulaCollapse(clickedNode);
                    }
                    break;
                case 3:
                    if (view.view.isConjunction(clickedNode))
                    {
                        view.view.formulaDecollapse(clickedNode);
                    }
                    view.view.getModel().setNodeStatus(clickedNode, 4);
                    break;  
                case 4:
                    if (view.view.isConjunction(clickedNode))
                    {
                        view.view.formulaCollapse(clickedNode);
                        view.view.getModel().setNodeStatus(clickedNode, 3);
                    }
                    break;  
            }
        }
        view.view.recalculate();
        view.updateDisplayAndRepaintFromEventDispatchThread();
    }
}
