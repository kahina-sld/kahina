package org.kahina.logic.sat.muc.visual;

import java.util.List;

import javax.swing.JComponent;

import org.kahina.core.control.KahinaEvent;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.muc.MUCStep;
import org.kahina.logic.sat.visual.cnf.list.KahinaSatInstanceListView;

public class MUCStepView extends KahinaSatInstanceListView
{
    MUCInstance kahina;
    
    public MUCStepView(MUCInstance kahina)
    {
        super(kahina);
        this.kahina = kahina;
    }
    
    @Override
    public JComponent makePanel()
    {
        MUCStepViewPanel panel = new MUCStepViewPanel(kahina);
        kahina.registerInstanceListener("redraw", panel);
        panel.setView(this);
        return panel;
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
        if (e.getSelectedStep() != -1)
        {
            MUCStep step = kahina.getState().retrieve(MUCStep.class, e.getSelectedStep());
            CnfSatInstance usInstance = kahina.getSatInstance().selectClauses(step.getUc());
            display(usInstance);
        }
        kahina.dispatchEvent(new KahinaRedrawEvent());
    }
}
