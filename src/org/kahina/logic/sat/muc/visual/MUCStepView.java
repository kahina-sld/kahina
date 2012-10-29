package org.kahina.logic.sat.muc.visual;

import java.util.List;

import javax.swing.JComponent;

import org.kahina.core.control.KahinaEvent;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.muc.MUCStep;
import org.kahina.logic.sat.muc.gui.ClauseSelectionEvent;
import org.kahina.logic.sat.visual.cnf.list.KahinaSatInstanceListView;

public class MUCStepView extends KahinaSatInstanceListView
{
    MUCInstance kahina;
    MUCStep currentStep;
    
    public MUCStepView(MUCInstance kahina)
    {
        super(kahina);
        this.kahina = kahina;
        this.currentStep = null;
    }
    
    @Override
    public JComponent makePanel()
    {
        MUCStepViewPanel panel = new MUCStepViewPanel(kahina);
        kahina.registerInstanceListener("redraw", panel);
        panel.setView(this);
        return panel;
    }
    
    public int getLineStatus(int lineID)
    {
        if (currentStep != null)
        {
            return currentStep.getIcStatus(currentStep.getUc().get(lineID));
        }
        return 0;
    }
    
    public void processEvent(KahinaEvent e)
    {
        if (e instanceof KahinaSelectionEvent)
        {
            processEvent((KahinaSelectionEvent) e);
        }
    }
    
    public void processEvent(KahinaSelectionEvent e)
    {
        if (model != null) 
        {
            recalculate();
            kahina.dispatchEvent(new KahinaRedrawEvent());
        }
    }

    
    public void recalculate()
    {
        int stepID = kahina.getState().getSelectedStepID();
        List<List<Integer>> clauses = model.getClauses();
        listModel.clear();
        if (stepID == -1)
        {
            for (int i = 1; i <= clauses.size(); i++)
            {
                StringBuilder s = new StringBuilder();
                s.append(i);
                s.append(": {");
                for (Integer literal : clauses.get(i-1))
                {
                    s.append(literal);
                    s.append(',');
                }
                s.deleteCharAt(s.length() - 1);
                s.append('}');
                listModel.addElement(s.toString());
            }
        }
        else
        {
            currentStep = kahina.getState().retrieve(MUCStep.class, stepID);
            for (int ic : currentStep.getUc())
            {
                StringBuilder s = new StringBuilder();
                s.append(ic);
                s.append(": {");
                for (Integer literal : clauses.get(ic))
                {
                    s.append(literal);
                    s.append(',');
                }
                s.deleteCharAt(s.length() - 1);
                s.append('}');
                listModel.addElement(s.toString());
            }
        }
    }
}
