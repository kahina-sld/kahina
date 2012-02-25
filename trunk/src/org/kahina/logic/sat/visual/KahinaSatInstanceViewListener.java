package org.kahina.logic.sat.visual;

import java.awt.event.MouseEvent;

import org.kahina.core.visual.graph.KahinaGraphViewListener;

public class KahinaSatInstanceViewListener extends KahinaGraphViewListener
{
    public KahinaSatInstanceViewListener(KahinaSatInstanceViewPanel view)
    {
        super(view);
    }

    protected void maybeShowPopup(MouseEvent e) 
    {
        if (e.isPopupTrigger()) 
        {
            KahinaSatInstanceViewContextMenu.getMenu(this, view.view).show(e.getComponent(),e.getX(), e.getY());
        }
    }
    
    protected void processExtensionCommand(String command)
    {
        KahinaSatInstanceView satView = (KahinaSatInstanceView) view.view;
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
