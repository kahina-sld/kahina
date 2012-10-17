package org.kahina.logic.sat.visual.cnf;

import java.awt.event.MouseEvent;

import org.kahina.core.visual.graph.KahinaGraphViewListener;

public class KahinaSatInstanceGraphViewListener extends KahinaGraphViewListener
{
    public KahinaSatInstanceGraphViewListener(KahinaSatInstanceGraphViewPanel view)
    {
        super(view);
    }

    protected void maybeShowPopup(MouseEvent e) 
    {
        if (e.isPopupTrigger()) 
        {
            KahinaSatInstanceGraphViewContextMenu.getMenu(this, view.view).show(e.getComponent(),e.getX(), e.getY());
        }
    }
    
    protected void processExtensionCommand(String command)
    {
        KahinaSatInstanceGraphView satView = (KahinaSatInstanceGraphView) view.view;
        if (command.equals("claByVar"))
        {
            satView.displayClausesByVariables();
        }
        else if (command.equals("claByLit"))
        {
            satView.displayClausesByLiterals();
        }
        else if (command.equals("claByCompLit"))
        {
            satView.displayClausesByComplementaryLiterals();
        }
        else if (command.equals("varByCla"))
        {
            satView.displayVariablesByClauses();
        }
        else if (command.equals("litByCla"))
        {
            satView.displayLiteralsByClauses();
        }
    }
}
