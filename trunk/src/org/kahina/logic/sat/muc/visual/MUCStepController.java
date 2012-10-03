package org.kahina.logic.sat.muc.visual;

import javax.swing.JComponent;

import org.kahina.core.control.KahinaEvent;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.visual.KahinaView;
import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.muc.MUCStep;

//it is possible to define views not bound to a data type derived from KahinaObject!
public class MUCStepController extends KahinaView
{
    //is interested in selection events!
    
    Integer[] ics;
    Integer[] icStatus;
    
    MUCInstance kahina;
            
    public MUCStepController(MUCInstance kahina)
    {
        super(kahina);
        this.kahina = kahina;
        ics = new Integer[0];
    }

    @Override
    public JComponent makePanel()
    {
        MUCStepControllerPanel panel = new MUCStepControllerPanel(kahina);
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
            ics = new Integer[step.getUc().size()];
            icStatus = new Integer[step.getUc().size()];
            int i = 0;
            for (Integer ic : step.getUc())
            {
                ics[i] = ic;
                icStatus[i] = step.getIcStatus(ic);
                i++;
            }
        }
        kahina.dispatchEvent(new KahinaRedrawEvent());
    }
    
    public void updateICStatus(MUCStep step)
    {
        int i = 0;
        for (Integer ic : step.getUc())
        {
            icStatus[i] = step.getIcStatus(ic);
            i++;
        }
    }
}
