package org.kahina.logic.sat.muc.visual;

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.LinkedList;
import java.util.List;
import java.util.TreeSet;

import javax.swing.JList;

import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.muc.MUCState;
import org.kahina.logic.sat.muc.MUCStep;
import org.kahina.logic.sat.muc.gui.ClauseSelectionEvent;
import org.kahina.logic.sat.muc.task.UCReductionTask;

public class PartitionBlockViewListener extends MouseAdapter implements ActionListener
{
    private final MUCInstance kahina;
    private final PartitionBlockViewPanel view;
    
    long lastClick = 0;
    public final static long DBL_CLICK_INTERVAL = 400;
    
    public PartitionBlockViewListener(MUCInstance kahina, PartitionBlockViewPanel view)
    {
        this.kahina = kahina;
        this.view = view;
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
                TreeSet<Integer> block = view.view.getModel().retrieveBlocks().get(listIndex);
                List<Integer> clauseIDs = new LinkedList<Integer>();
                for (int literal : block)
                {
                    clauseIDs.add(-literal);
                }
                long time = System.currentTimeMillis();
                //System.err.println("label: " + label + " interval: " + (time - lastClick) + " = " + time + " - " + lastClick);
                if (time - lastClick > DBL_CLICK_INTERVAL)
                {
                    lastClick = time;
                    kahina.dispatchEvent(new ClauseSelectionEvent(clauseIDs));
                }
                else
                {
                    System.err.println("Reducing by clause IDs: " + clauseIDs);
                    lastClick = time;
                    reduce(clauseIDs);
                }
            }
        }
    }
    
    private void reduce(List<Integer> ics)
    {
        MUCState state = kahina.getState();
        MUCStep ucStep = state.retrieve(MUCStep.class, state.getSelectedStepID());
        UCReductionTask redTask = new UCReductionTask(  null, kahina.getReductionManager(), state.getStatistics(), 
                                                        ucStep, state.getSelectedStepID(), 
                                                        ics, state.getFiles(), state.getSatInstance()
                                                      );
        kahina.getReductionManager().addTask(redTask);
    }

    @Override
    public void actionPerformed(ActionEvent arg0)
    {
        // TODO Auto-generated method stub   
    }
}
