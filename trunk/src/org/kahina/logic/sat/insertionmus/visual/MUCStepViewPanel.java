package org.kahina.logic.sat.insertionmus.visual;


import org.kahina.core.control.KahinaEvent;
import org.kahina.logic.sat.insertionmus.MUCInstance;
import org.kahina.logic.sat.insertionmus.gui.ClauseSelectionEvent;
import org.kahina.logic.sat.visual.cnf.list.KahinaSatInstanceListViewPanel;

public class MUCStepViewPanel extends KahinaSatInstanceListViewPanel
{
    MUCInstance kahina;
    MUCStepViewRemaining view;
    
    public MUCStepViewPanel(MUCInstance kahina)
    {
        super();
        this.kahina = kahina;
        kahina.registerSessionListener("clauseSelection", this);
        getList().addMouseListener(new MUCStepViewListener(kahina, this));
    }
    
    public void processEvent(KahinaEvent e)
    {
        if (e instanceof ClauseSelectionEvent)
        {
            processEvent((ClauseSelectionEvent) e);
        }
        else
        {
            super.processEvent(e);
        }
    }
    
    public void processEvent(ClauseSelectionEvent e)
    {
        if (view != null && view.currentStep != null) 
        {
            int selectedClauses = e.getClauseID();
            getList().getSelectionModel().clearSelection();
//            for (int i = 0; i < view.currentStep.getSice().size(); i++)
//            {
//                if (selectedClauses.contains(view.currentStep.getSice().get(i)))
//                {
//                    getList().getSelectionModel().addSelectionInterval(i, i);
//                }
//            }
            getList().repaint();
        }
    }
    
    public void setView(MUCStepViewRemaining view)
    {
        super.setView(view);
        this.view = view;
    }
    
    public void selectAll()
    {
        if (view.currentStep != null)
        {
//            getList().setSelectionInterval(0, view.currentStep.getSice().size() - 1);
//            getList().repaint();
        }
    }
}
