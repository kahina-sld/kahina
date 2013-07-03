package org.kahina.logic.sat.insertionmus.visual;

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import java.util.LinkedList;
import java.util.List;


import javax.swing.JList;
import javax.swing.JOptionPane;

import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.insertionmus.MUCInstance;
import org.kahina.logic.sat.insertionmus.MUCState;
import org.kahina.logic.sat.insertionmus.MUCStep;
//import org.kahina.logic.sat.insertionmus.MUCStepType;
import org.kahina.logic.sat.insertionmus.gui.ClauseSelectionEvent;

public class MUCStepViewListener extends MouseAdapter implements ActionListener
{
	private final MUCInstance kahina;
	private final MUCStepViewPanel view;

	long lastClick = 0;
	//	private MUCState state;
	public final static long DBL_CLICK_INTERVAL = 200;

	public MUCStepViewListener(MUCInstance kahina, MUCStepViewPanel view)
	{
		this.kahina = kahina;
		this.view = view;
		//		this.state = ;
	}

	@Override
	public void mouseClicked(MouseEvent e)
	{
		if (e.getSource() instanceof JList)
		{
			//			System.out.println("Click1");
			int listIndex = ((JList) e.getSource()).locationToIndex(new Point(e.getX(), e.getY()));
			//        	System.out.println(listIndex + " " + e);
			MUCState state = kahina.getState();
			MUCStep uc = state.getSelectedStep();
			if (uc != null && listIndex >= 0)
			{
				long time = System.currentTimeMillis();
				//System.err.println("label: " + label + " interval: " + (time - lastClick) + " = " + time + " - " + lastClick);
				if (time - lastClick < DBL_CLICK_INTERVAL)
				{
					lastClick = 0;
					int clauseIndex = uc.getUc()[listIndex];

					kahina.dispatchEvent(new ClauseSelectionEvent(clauseIndex));
					
					if (state.getAlgorithm().nextStep(clauseIndex, uc.getData())){
						MUCStep nextStep = new MUCStep(uc.getData());
						uc.reset();

						if (!kahina.getState().stepExists(nextStep)){
							kahina.getState().newStep(nextStep, uc.getID());
						}
//						uc = nextStep;
						//						kahina.dispatchEvent()
					}
					//					kahina.dispatchEvent(new )
//
					kahina.dispatchEvent(new KahinaUpdateEvent(uc.getID()));
					kahina.dispatchEvent(new KahinaSelectionEvent(uc.getID()));
//					kahina.dispatchEvent(new KahinaUpdateEvent(nextStep.getID()));
//					kahina.dispatchEvent(new KahinaSelectionEvent(nextStep.getID()));
					//				if (e.getClickCount()> 2){
					System.out.println(clauseIndex);
					//					List<Integer> clauseIDs = new LinkedList<Integer>();
					//					clauseIDs.add(clauseIndex);
					//					kahina.dispatchEvent(new ClauseSelectionEvent(clauseIDs));
				}
				else
				{
					//					reduce(ic, false);
					lastClick = time;
				}
			}
		}
	}

//	private void reduce(int ic, boolean modelRotation)
//	{
//		MUCState state = kahina.getState();
//		MUCStep ucStep = state.retrieve(MUCStep.class, state.getSelectedStepID());
//		List<Integer> cands = new LinkedList<Integer>();
//		cands.add(ic);
//		//        ReductionTask redTask = new ReductionTask(  null, kahina.getReductionManager(), state.getStatistics(), 
//		//                                                        ucStep, state.getSelectedStepID(), 
//		//                                                        cands, state.getFiles(), state.getSatInstance()
//		//                                                      );
//		//        redTask.setModelRotation(modelRotation);
//		//        kahina.getReductionManager().addTask(redTask);
//	}

	private void reduce(List<Integer> ics)
	{
		//TODO reduction
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
//			processSubselection(s.substring(9));
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
			//            if (uc != null)
			//            {
			//                int ic = uc.getSice().get(listIndex);
			//                reduce(ic, true);
			//            }
		}
		else if (s.startsWith("reduce"))
		{
			int listIndex = Integer.parseInt(s.substring(6));
			MUCStep uc = kahina.getState().getSelectedStep();
			//            if (uc != null)
			//            {
			//                int ic = uc.getSice().get(listIndex);
			//                reduce(ic, false);
			//            }
		}
		else if (s.equals("redSelOnce"))
		{
			List<Integer> ics = new LinkedList<Integer>();
			int[] redList = view.getList().getSelectedIndices();
			MUCStep uc = kahina.getState().getSelectedStep();
			if (uc != null)
			{
				//                for (int listIndex : redList)
				//                {
				//                    int ic = uc.getSice().get(listIndex);
				//                    ics.add(ic);
				//                }
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
				//                for (int listIndex : redList)
				//                {
				//                    int ic = uc.getSice().get(listIndex);
				//                    System.err.println("   semi-automatic reduction of selection ID " + listIndex + " i.e. clause " + ic);
				//                    reduce(ic, false);
				//                }
			}
		}
		else if (s.equals("leanKernel"))
		{
			MUCState state = kahina.getState();
			//            int newStepID = state.autarkyReduction(state.getSelectedStepID());
			//            kahina.dispatchInstanceEvent(new KahinaSelectionEvent(newStepID));        
		}
	}

//	

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
		System.err.println("size of selected US: " + uc.getSize());
		if (uc != null)
		{
			for (int index : selection)
			{
				//                if (uc.getIcStatus(uc.getUc().get(index)) == desiredStatus)
				//                {
				//                    selectionList.add(index);
				//                }
			}
		}
		return toIntArray(selectionList);
	}

	//    private int[] processSizeSubselection(String size, int[] selection)
	//    {
	//        List<Integer> selectionList = new LinkedList<Integer>();
	//        MUCStep uc = kahina.getState().getSelectedStep();
	//        CnfSatInstance instance =  kahina.getState().getSatInstance();
	//        if (uc != null)
	//        {
	//            if (size.equals("Large"))
	//            {
	//                for (int index : selection)
	//                {
	//                    int clauseID = uc.getUc().get(index);
	//                    if (instance.getClause(clauseID-1).size() > 5)
	//                    {
	//                        selectionList.add(index);
	//                    }
	//                }
	//            }
	//            else
	//            {         
	//                int desiredSize = Integer.parseInt(size);
	//                for (int index : selection)
	//                {
	//                    int clauseID = uc.getUc().get(index);
	//                    if (instance.getClause(clauseID-1).size() == desiredSize)
	//                    {
	//                        selectionList.add(index);
	//                    }
	//                }
	//            }
	//        }
	//        return toIntArray(selectionList);
	//    }

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
					int clauseID = uc.getUc()[index];
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
