package org.kahina.logic.sat.muc.visual;

import java.awt.event.MouseEvent;
import java.util.LinkedList;
import java.util.List;
import java.util.TreeSet;

import org.kahina.core.visual.tree.KahinaTreeViewListener;
import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.muc.MUCState;
import org.kahina.logic.sat.muc.MUCStep;
import org.kahina.logic.sat.muc.gui.ClauseSelectionEvent;
import org.kahina.logic.sat.muc.task.UCReductionTask;

public class RecursiveBlockViewListener extends KahinaTreeViewListener
{
    MUCInstance kahina;
    RecursiveBlockViewPanel view;
    
    public final static long DBL_CLICK_INTERVAL = 500;
    
    public RecursiveBlockViewListener(RecursiveBlockViewPanel view, MUCInstance kahina)
    {
        super(view, kahina);
        this.kahina = kahina;
        this.view = view;
    }
    
    @Override
    public void mouseClicked(MouseEvent e)
    {
        int clickedNode = view.view.nodeAtCoordinates(e.getX(), e.getY());
        MUCStep uc = kahina.getState().getSelectedStep();
        if (clickedNode != -1 && uc != null)
        {
            TreeSet<Integer> block = view.view.getBlockHandler().getBlock(clickedNode);
            List<Integer> clauseIDs = new LinkedList<Integer>();
            for (int literal : block)
            {
                clauseIDs.add(-literal);
            }
            if (lastMouseEvent != null && e.getWhen() - lastMouseEvent.getWhen() < DBL_CLICK_INTERVAL)
            {
                System.err.println("Reducing by clause IDs: " + clauseIDs);
                for (int clauseID : clauseIDs)
                {
                    List<Integer> clauseList = new LinkedList<Integer>();
                    clauseList.add(clauseID);
                    reduce(clauseList);
                }
                lastMouseEvent = null;
            }
            else
            {
                //view.view.setMarkedNode(clickedNode);
                kahina.dispatchEvent(new ClauseSelectionEvent(clauseIDs));
                lastMouseEvent = e;
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

}
