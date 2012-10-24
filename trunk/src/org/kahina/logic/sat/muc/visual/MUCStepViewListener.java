package org.kahina.logic.sat.muc.visual;

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.JLabel;
import javax.swing.JList;

import org.kahina.core.visual.graph.KahinaGraphViewContextMenu;
import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.muc.MUCState;
import org.kahina.logic.sat.muc.MUCStep;
import org.kahina.logic.sat.muc.gui.ClauseSelectionEvent;
import org.kahina.logic.sat.muc.task.UCReductionTask;

public class MUCStepViewListener extends MouseAdapter implements ActionListener
{
    private final MUCInstance kahina;
    private final MUCStepViewPanel view;
    
    long lastClick = 0;
    public final static long DBL_CLICK_INTERVAL = 200;
    
    public MUCStepViewListener(MUCInstance kahina, MUCStepViewPanel view)
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
                    reduce(ic);
                    lastClick = 0;
                }
            }
        }
    }
    
    private void reduce(int ic)
    {
        MUCState state = kahina.getState();
        MUCStep ucStep = state.retrieve(MUCStep.class, state.getSelectedStepID());
        UCReductionTask redTask = new UCReductionTask(  null, kahina.getReductionManager(), state.getStatistics(), 
                                                        ucStep, state.getSelectedStepID(), 
                                                        ic, state.getFiles()
                                                      );
        kahina.getReductionManager().addTask(redTask);
    }
    
    public void mousePressed(MouseEvent e)
    {
        maybeShowPopup(e);
    }
    
    public void mouseReleased(MouseEvent e)
    {
        maybeShowPopup(e);
    }
    
    protected void maybeShowPopup(MouseEvent e) 
    {
        if (e.isPopupTrigger()) 
        {
            int listIndex = ((JList) e.getSource()).locationToIndex(new Point(e.getX(), e.getY()));
            MUCStepViewContextMenu.getMenu(this, view, kahina, listIndex).show(e.getComponent(),e.getX(), e.getY());
        }
    }

    @Override
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("selectAll"))
        {
            view.selectAll();
        }
        else if (s.equals("selectDialog"))
        {
            //TODO
        }
        else if (s.startsWith("reduce"))
        {
            int listIndex = Integer.parseInt(s.substring(6));
            MUCStep uc = kahina.getState().getSelectedStep();
            if (uc != null)
            {
                int ic = uc.getUc().get(listIndex);
                reduce(ic);
            }
        }
        else if (s.startsWith("reduceMR"))
        {
            int listIndex = Integer.parseInt(s.substring(8));
            //TODO
        }
        else if (s.equals("redSelOnce"))
        {
            //TODO
        }
        else if (s.equals("redSelIndiv"))
        {
            int[] redList = view.getList().getSelectedIndices();
            MUCStep uc = kahina.getState().getSelectedStep();
            if (uc != null)
            {
                for (int listIndex : redList)
                {
                    int ic = uc.getUc().get(listIndex);
                    reduce(ic);
                }
            }
        }
        else if (s.equals("findAutarkies"))
        {
            //TODO
        }
    }
}
