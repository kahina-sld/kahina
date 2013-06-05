package org.kahina.logic.sat.insertionmus.visual;

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;

import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.visual.graph.KahinaGraphViewContextMenu;
import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.insertionmus.MUCInstance;
import org.kahina.logic.sat.insertionmus.MUCState;
import org.kahina.logic.sat.insertionmus.MUCStep;
//import org.kahina.logic.sat.insertionmus.MUCStepType;
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
//        ReductionTask redTask = new ReductionTask(  null, kahina.getReductionManager(), state.getStatistics(), 
//                                                        ucStep, state.getSelectedStepID(), 
//                                                        cands, state.getFiles(), state.getSatInstance()
//                                                      );
//        redTask.setModelRotation(modelRotation);
//        kahina.getReductionManager().addTask(redTask);
    }
    
    private void reduce(List<Integer> ics)
    {
        MUCState state = kahina.getState();
        MUCStep ucStep = state.retrieve(MUCStep.class, state.getSelectedStepID());
//        ReductionTask redTask = new ReductionTask(  null, kahina.getReductionManager(), state.getStatistics(), 
//                                                        ucStep, state.getSelectedStepID(), 
//                                                        ics, state.getFiles(), state.getSatInstance()
//                                                      );
//        kahina.getReductionManager().addTask(redTask);
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
        if (s.startsWith("subselect"))
        {
            processSubselection(s.substring(9));
        }
        else if (s.equals("selectAll"))
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
                System.err.println("Received user command to simultaneously reduce by the following clause IDs US #" 
                        + kahina.getState().getSelectedStepID() + ":");
                System.err.println(ics.toString());
                reduce(ics);
            }
        }
        else if (s.equals("redSelIndiv"))
        {
            int[] redList = view.getList().getSelectedIndices();
            System.err.println("Received user command to individually reduce at the following selection indices in US #" 
                               + kahina.getState().getSelectedStepID() + ":");
            System.err.println(redList.toString());
            MUCStep uc = kahina.getState().getSelectedStep();
            if (uc != null)
            {
                for (int listIndex : redList)
                {
                    int ic = uc.getUc().get(listIndex);
                    System.err.println("   semi-automatic reduction of selection ID " + listIndex + " i.e. clause " + ic);
                    reduce(ic, false);
                }
            }
        }
        else if (s.equals("leanKernel"))
        {
            MUCState state = kahina.getState();
            int newStepID = state.autarkyReduction(state.getSelectedStepID());
            kahina.dispatchInstanceEvent(new KahinaSelectionEvent(newStepID));        
        }
    }
    
    private void processSubselection(String subselectionCommand)
    {
        int[] selection = view.getList().getSelectedIndices();
        int[] newSelection;
        if (subselectionCommand.startsWith("Status"))
        {
            newSelection = processStatusSubselection(subselectionCommand.substring(6), selection);
        }
        else if (subselectionCommand.startsWith("Size"))
        {
            newSelection = processSizeSubselection(subselectionCommand.substring(4), selection);
        }
        else if (subselectionCommand.startsWith("First"))
        {
            newSelection = processFirstSubselection(subselectionCommand.substring(5), selection);
        }
        else if (subselectionCommand.startsWith("Last"))
        {
            newSelection = processLastSubselection(subselectionCommand.substring(4), selection);
        }
        else if (subselectionCommand.startsWith("Random"))
        {
            newSelection = processRandomSubselection(subselectionCommand.substring(6), selection);
        }
        else if (subselectionCommand.equals("Literal"))
        {
            newSelection = getLiteralSubselection(selection);
        }
        else
        {
            System.err.println("WARNING: unkown subselection command \"" + subselectionCommand + "\"!");
            return;
        }
        view.getList().setSelectedIndices(newSelection);
    }
    
    private int[] processStatusSubselection(String status, int[] selection)
    {
        List<Integer> selectionList = new LinkedList<Integer>();
        int desiredStatus = -1;
        if (status.equals("Unknown"))
        {
            desiredStatus = 0;
        }
        else if (status.equals("FallAway"))
        {
            desiredStatus = 3;
        }
        else if (status.equals("Reduced"))
        {
            desiredStatus = 1;
        }
        else if (status.equals("Critical"))
        {
            desiredStatus = 2;
        }
        else
        {
            System.err.println("WARNING: unkown status \"" + status + "\" in subselection command!");
            return selection;
        }
        MUCStep uc = kahina.getState().getSelectedStep();
        System.err.println("size of selected US: " + uc.getUc().size());
        if (uc != null)
        {
            for (int index : selection)
            {
                if (uc.getIcStatus(uc.getUc().get(index)) == desiredStatus)
                {
                    selectionList.add(index);
                }
            }
        }
        return toIntArray(selectionList);
    }
    
    private int[] processSizeSubselection(String size, int[] selection)
    {
        List<Integer> selectionList = new LinkedList<Integer>();
        MUCStep uc = kahina.getState().getSelectedStep();
        CnfSatInstance instance =  kahina.getState().getSatInstance();
        if (uc != null)
        {
            if (size.equals("Large"))
            {
                for (int index : selection)
                {
                    int clauseID = uc.getUc().get(index);
                    if (instance.getClause(clauseID-1).size() > 5)
                    {
                        selectionList.add(index);
                    }
                }
            }
            else
            {         
                int desiredSize = Integer.parseInt(size);
                for (int index : selection)
                {
                    int clauseID = uc.getUc().get(index);
                    if (instance.getClause(clauseID-1).size() == desiredSize)
                    {
                        selectionList.add(index);
                    }
                }
            }
        }
        return toIntArray(selectionList);
    }
    
    private int[] processFirstSubselection(String number, int[] selection)
    {
        List<Integer> selectionList = new LinkedList<Integer>();   
        int desiredNumber = Integer.parseInt(number);
        for (int i = 0; i < desiredNumber; i++)
        {
            selectionList.add(selection[i]);
        }
        return toIntArray(selectionList);
    }
    
    private int[] processLastSubselection(String number, int[] selection)
    {
        List<Integer> selectionList = new LinkedList<Integer>();
        int desiredNumber = Integer.parseInt(number);
        for (int i = selection.length - 1; i >= selection.length - desiredNumber; i--)
        {
            selectionList.add(selection[i]);
        }
        return toIntArray(selectionList);
    }
    
    private int[] processRandomSubselection(String number, int[] selection)
    {
        int desiredNumber = Integer.parseInt(number);
        if (desiredNumber > selection.length) desiredNumber = selection.length;
        //generate a permutation
        ArrayList<Integer> permutation = new ArrayList<Integer>(selection.length);
        permutation.add(selection[0]);
        Random rnd = new Random();
        for (int i = 1; i < selection.length; i++)
        {
            permutation.add(rnd.nextInt(permutation.size()), selection[i]);
        }
        //then take the the desired number from the beginning
        return toIntArray(permutation.subList(0, desiredNumber));
    }
    
    public int[] getLiteralSubselection(int[] selection)
    {
        List<Integer> selectionList = new LinkedList<Integer>();
        String litString = JOptionPane.showInputDialog("Enter a literal to subselect all clauses containing it.");
        try
        {
            Integer lit = Integer.parseInt(litString);
            MUCStep uc = kahina.getState().getSelectedStep();
            CnfSatInstance instance =  kahina.getState().getSatInstance();
            if (uc != null)
            {
                for (int index : selection)
                {
                    int clauseID = uc.getUc().get(index);
                    if (instance.getClause(clauseID-1).contains(lit))
                    {
                        selectionList.add(index);
                    }
                }
            }
        }
        catch (NumberFormatException e)
        {
            return selection;
        }
        return toIntArray(selectionList);
    }
    
    private int[] toIntArray(List<Integer> list)
    {
        int[] array = new int[list.size()];
        int i = 0;
        for (int listItem : list)
        {
            array[i] = listItem;
            i++;
        }
        return array;
    }
}
