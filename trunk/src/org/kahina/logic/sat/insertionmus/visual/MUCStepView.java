package org.kahina.logic.sat.insertionmus.visual;

import java.util.List;

import javax.swing.JComponent;
import javax.swing.event.ListDataListener;

import org.kahina.core.control.KahinaEvent;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.insertionmus.MUCInstance;
import org.kahina.logic.sat.insertionmus.MUCStep;
import org.kahina.logic.sat.insertionmus.gui.ClauseSelectionEvent;
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
            if (lineID >= currentStep.getUc().size())
            {
                //TODO: this happens twice until long term solution: implement FastListModel
                //System.err.println("WARNING: had to prevent getLineStatus(" + lineID + ")");
                return 0;
            }
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
        else
        {
            super.processEvent(e);
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
        kahina.getLogger().startMeasuring();
        int stepID = kahina.getState().getSelectedStepID();
        //the list model must be taken offline
        //to prevent redraws from being triggered after each added element
        ListDataListener[] listeners = listModel.getListDataListeners();
        for (ListDataListener listener : listeners)
        {
            listModel.removeListDataListener(listener);
        }
        listModel.clear();
        if (stepID == -1)
        {
            for (int i = 1; i <= model.getSize(); i++)
            {
                StringBuilder s = new StringBuilder();
                s.append(i);
                s.append(": {");
                for (Integer literal : model.getClause(i-1))
                {
                    s.append(kahina.getSatInstance().getSymbolForLiteral(literal));
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
                for (Integer literal : model.getClause(ic - 1))
                {
                    s.append(kahina.getSatInstance().getSymbolForLiteral(literal));
                    s.append(',');
                }
                s.deleteCharAt(s.length() - 1);
                s.append('}');
                listModel.addElement(s.toString());
            }
        }
        for (ListDataListener listener : listeners)
        {
            listModel.addListDataListener(listener);
        }
        //TODO: hack forces JList to update, long term solution: implement FastListModel
        listModel.add(0, "dummy for update");
        listModel.remove(0);
        needsRedraw = true;
        kahina.getLogger().endMeasuring("for recalculating the MUCStepView");
    }
}
