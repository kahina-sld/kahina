package org.kahina.logic.sat.muc.visual;

import java.awt.Point;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.JLabel;
import javax.swing.JList;

import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.muc.MUCState;
import org.kahina.logic.sat.muc.MUCStep;
import org.kahina.logic.sat.muc.gui.ClauseSelectionEvent;
import org.kahina.logic.sat.muc.task.UCReductionTask;

public class MUCStepViewPanelMouseListener extends MouseAdapter
{
    private final MUCInstance kahina;
    
    long lastClick = 0;
    public final static long DBL_CLICK_INTERVAL = 200;
    
    public MUCStepViewPanelMouseListener(MUCInstance kahina)
    {
        this.kahina = kahina;
    }
    
    @Override
    public void mouseClicked(MouseEvent e)
    {
        if (e.getSource() instanceof JList)
        {
            int listIndex = ((JList) e.getSource()).locationToIndex(new Point(e.getX(), e.getY()));
            MUCStep uc = kahina.getState().getSelectedStep();
            if (uc != null)
            {
                int ic = uc.getUc().get(listIndex);
                long time = System.currentTimeMillis();
                //System.err.println("label: " + label + " interval: " + (time - lastClick) + " = " + time + " - " + lastClick);
                if (time - lastClick > DBL_CLICK_INTERVAL)
                {
                    lastClick = time;
                    kahina.dispatchEvent(new ClauseSelectionEvent(ic));
                }
                else
                {
                    MUCState state = kahina.getState();
                    MUCStep ucStep = state.retrieve(MUCStep.class, state.getSelectedStepID());
                    UCReductionTask redTask = new UCReductionTask(  null, kahina.getReductionManager(), state.getStatistics(), 
                                                                    ucStep, state.getSelectedStepID(), 
                                                                    ic, state.getFiles()
                                                                  );
                    kahina.getReductionManager().addTask(redTask);
                    //kahina.dispatchEvent(new KahinaControlEvent(label + ""));
                    lastClick = 0;
                }
            }
        }
    }
}
