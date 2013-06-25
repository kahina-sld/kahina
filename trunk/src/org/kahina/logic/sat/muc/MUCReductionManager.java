package org.kahina.logic.sat.muc;

import java.lang.reflect.InvocationTargetException;
import java.util.ConcurrentModificationException;
import java.util.LinkedList;
import java.util.List;
import java.util.TreeSet;

import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.core.task.KahinaTask;
import org.kahina.core.task.KahinaTaskManager;
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.logic.sat.io.minisat.MiniSATFiles;
import org.kahina.logic.sat.muc.data.Overlap;
import org.kahina.logic.sat.muc.task.ReductionTask;

public class MUCReductionManager extends KahinaTaskManager
{
    MUCInstance kahina;
    
    MiniSATFiles files;
    
    public MUCReductionManager(MUCInstance kahina, MiniSATFiles files)
    {
        super();
        this.kahina = kahina;
        
        this.files = files;
    }
    
    public void taskFinished(KahinaTask task)
    {
        try
        {
            super.taskFinished(task);
            if (task instanceof ReductionTask)
            {
                MUCState state = kahina.getState();
                ReductionTask ucTask = (ReductionTask) task;
                MUCStep result = ucTask.getResult();
                int resultID = -1;
                //attempt was unsuccessful
                if (ucTask.uc == result)
                {        
                    resultID = ucTask.ucID;
                    System.err.println("failed reduction attempt: clause(s) " + ucTask.candidates + " in US " + resultID);
                    //uc and ucID just stay the same
                    if (!kahina.getState().usesMetaLearning())
                    {
                        if (ucTask.candidates.size() == 1)
                        {
                            state.addAndDistributeIrreducibilityInfo(ucTask.ucID, ucTask.candidates.get(0));
                        }
                    }
                    else
                    {
                        //we learn that the current selector variables cannot be 1 together
                        TreeSet<Integer> metaClause = new TreeSet<Integer>();
                        int numClauses = state.getStatistics().numClausesOrGroups;
                        for (int i = 1; i <= numClauses; i++)
                        {
                            if (!result.getUc().contains(i) || ucTask.candidates.contains(i))
                            {
                                metaClause.add(i);
                            }
                        }
                        //state.learnMetaBlock(metaClause);
                        state.learnMetaClause(metaClause);
                    }
                    if (!ucTask.isDummyTask())
                    {
                        files.deleteTempFiles();
                    }
                    //model rotation if only one candidate was reduced, and the task was configured to apply MR
                    if (ucTask.usesModelRotation() && ucTask.candidates.size() == 1)
                    {
                        TreeSet<Integer> derivedCritical = new TreeSet<Integer>();
                        state.modelRotation(ucTask.getModel(), ucTask.ucID, ucTask.candidates.get(0), derivedCritical);
                        System.err.println("  model rotation yields additional critical clauses " + derivedCritical);
                    }
                }
                //attempt was successful, we might have arrived at a new UC
                else
                {
                    System.err.println("successful reduction: clause(s) " + ucTask.candidates + " in US " + ucTask.ucID);
                    int stepID = state.registerMUC(result, ucTask.ucID, ucTask.candidates);
                    resultID = stepID;
                    System.err.println("uc: " + ucTask.uc.getUc());
                    System.err.println("result: " + result.getUc());
                    Overlap overlap = new Overlap(ucTask.uc.getUc(),result.getUc());
                    for (int candidate : overlap.aMinusB)
                    {
                        if (ucTask.uc.getRemovalLink(candidate) == null)
                        {
                            state.addAndDistributeReducibilityInfo(ucTask.ucID, candidate, -2);
                        }
                    }
                    if (overlap.aMinusB.size() > 1)
                    {
                        System.err.println("  clause set refinement removes " + (overlap.aMinusB.size() - 1) + " additional clause(s)");
                    }
                    if (ucTask.candidates.size() == 1)
                    {
                        ucTask.uc.setRemovalLink(ucTask.candidates.get(0), stepID);
                    }
                    if (!ucTask.isDummyTask())
                    {
                        files.deleteTempFiles();
                    }
                    MUCStep uc = state.retrieve(MUCStep.class, stepID);
                    if (kahina.getState().usesMetaLearning())
                    {
                        state.learnMetaUnits(uc);
                        if (kahina.getState().usesBlocks())
                        {
                            //we ensure that the meta instance can compactly represent the new UC
                            TreeSet<Integer> metaBlock = new TreeSet<Integer>();
                            int numClauses = state.getStatistics().numClausesOrGroups;
                            for (int i = 1; i <= numClauses; i++)
                            {
                                if (!result.getUc().contains(i))
                                {
                                    metaBlock.add(i);
                                }
                            }
                            state.learnMetaBlock(metaBlock);
                        }
                    }
                }
                state.updateDecisionNode(ucTask.ucID);
                if (kahina.getState().usesBlocks())
                {
                    kahina.getGUI().getViewByID("currentUCBlocks").getModel().requireUpdate();
                }
                kahina.getGUI().getViewByID("currentUC").requireRedraw();
                kahina.dispatchInstanceEvent(new KahinaSelectionEvent(resultID));
                //kahina.dispatchInstanceEvent(new KahinaUpdateEvent(kahina.getState().getSelectedStepID()));
                //kahina.dispatchInstanceEvent(new KahinaRedrawEvent());
            }
        }
        catch (NullPointerException e)
        {
            System.err.println("WARNING: caught NullPointerException in MUCReductionManager.taskFinished():");
            e.printStackTrace();
        }
        catch (IndexOutOfBoundsException e)
        {
            System.err.println("WARNING: caught IndexOutOfBoundsException in MUCReductionManager.taskFinished():");
            e.printStackTrace();
        }
        catch (ConcurrentModificationException e)
        {
            System.err.println("WARNING: caught ConcurrentModificationException in MUCReductionManager.taskFinished():");
            e.printStackTrace();
        }
    }
}
