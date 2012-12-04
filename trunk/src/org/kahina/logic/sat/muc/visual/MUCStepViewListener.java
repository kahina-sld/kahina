package org.kahina.logic.sat.muc.visual;

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import javax.swing.JLabel;
import javax.swing.JList;

import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.visual.graph.KahinaGraphViewContextMenu;
import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.muc.MUCState;
import org.kahina.logic.sat.muc.MUCStep;
import org.kahina.logic.sat.muc.data.Overlap;
import org.kahina.logic.sat.muc.gui.ClauseSelectionEvent;
import org.kahina.logic.sat.muc.task.ReductionTask;

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
                    List<Integer> clauseIDs = new LinkedList<Integer>();
                    clauseIDs.add(ic);
                    kahina.dispatchEvent(new ClauseSelectionEvent(clauseIDs));
                }
                else
                {
                    reduce(ic, false);
                    lastClick = 0;
                }
            }
        }
    }
    
    private void reduce(int ic, boolean modelRotation)
    {
        MUCState state = kahina.getState();
        MUCStep ucStep = state.retrieve(MUCStep.class, state.getSelectedStepID());
        List<Integer> cands = new LinkedList<Integer>();
        cands.add(ic);
        ReductionTask redTask = new ReductionTask(  null, kahina.getReductionManager(), state.getStatistics(), 
                                                        ucStep, state.getSelectedStepID(), 
                                                        cands, state.getFiles(), state.getSatInstance()
                                                      );
        redTask.setModelRotation(modelRotation);
        kahina.getReductionManager().addTask(redTask);
    }
    
    private void reduce(List<Integer> ics)
    {
        MUCState state = kahina.getState();
        MUCStep ucStep = state.retrieve(MUCStep.class, state.getSelectedStepID());
        ReductionTask redTask = new ReductionTask(  null, kahina.getReductionManager(), state.getStatistics(), 
                                                        ucStep, state.getSelectedStepID(), 
                                                        ics, state.getFiles(), state.getSatInstance()
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
        else if (s.startsWith("reduceMR"))
        {
            int listIndex = Integer.parseInt(s.substring(8));
            MUCStep uc = kahina.getState().getSelectedStep();
            if (uc != null)
            {
                int ic = uc.getUc().get(listIndex);
                reduce(ic, true);
            }
        }
        else if (s.startsWith("reduce"))
        {
            int listIndex = Integer.parseInt(s.substring(6));
            MUCStep uc = kahina.getState().getSelectedStep();
            if (uc != null)
            {
                int ic = uc.getUc().get(listIndex);
                reduce(ic, false);
            }
        }
        else if (s.equals("redSelOnce"))
        {
            List<Integer> ics = new LinkedList<Integer>();
            int[] redList = view.getList().getSelectedIndices();
            MUCStep uc = kahina.getState().getSelectedStep();
            if (uc != null)
            {
                for (int listIndex : redList)
                {
                    int ic = uc.getUc().get(listIndex);
                    ics.add(ic);
                }
                reduce(ics);
            }
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
                    reduce(ic, false);
                }
            }
        }
        else if (s.equals("leanKernel"))
        {
            MUCState state = kahina.getState();
            
            //extract the lean kernel from the currently selected US
            MUCStep uc = state.getSelectedStep();
            int ucID = state.getSelectedStepID();
            CnfSatInstance leanKernelUC = kahina.getSatInstance().selectClauses(uc.getUc()).copy();
            leanKernelUC.reduceToLeanKernel();
            
            //generate a new US representing the lean kernel 
            MUCStep leanUc = new MUCStep();
            List<Integer> leanUS = leanUc.getUc();
            Map<String,Integer> idMap = kahina.getSatInstance().generateClauseToIDMap();
            StringBuilder clauseRepresentation;
            for (List<Integer> clause : leanKernelUC.getClauses())
            {
                clauseRepresentation = new StringBuilder();
                for (int lit : clause)
                {
                    clauseRepresentation.append(lit + ".");
                }
                int a = idMap.get(clauseRepresentation.toString());
                if (!kahina.getSatInstance().isDontCareClause(a))
                {
                    leanUS.add(a+1);
                }
            }
            
            //add a node with the lean kernel US to the reduction graph
            int resultID = state.registerMUC(leanUc, ucID, new LinkedList<Integer>());
            if (resultID != ucID)
            {
                Overlap overlap = new Overlap(uc.getUc(),leanUc.getUc());
                for (int candidate : overlap.aMinusB)
                {
                    if (uc.getRemovalLink(candidate) == null)
                    {
                        state.addAndDistributeReducibilityInfo(ucID, candidate, -2);
                    }
                }
                state.updateDecisionNode(ucID);
                kahina.dispatchInstanceEvent(new KahinaSelectionEvent(resultID));
            }
        }
    }
}
