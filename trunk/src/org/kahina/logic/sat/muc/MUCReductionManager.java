package org.kahina.logic.sat.muc;

import java.util.LinkedList;
import java.util.List;

import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.core.task.KahinaTask;
import org.kahina.core.task.KahinaTaskManager;
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.logic.sat.muc.task.UCReductionTask;

public class MUCReductionManager extends KahinaTaskManager
{
    MUCInstance kahina;
    
    public MUCReductionManager(MUCInstance kahina)
    {
        super();
        this.kahina = kahina;
    }
    
    public void taskFinished(KahinaTask task)
    {
        super.taskFinished(task);
        if (task instanceof UCReductionTask)
        {
            MUCState state = kahina.getState();
            UCReductionTask ucTask = (UCReductionTask) task;
            MUCStep result = ucTask.getResult();
            //attempt was unsuccessful
            if (ucTask.uc == result)
            {
                //uc and ucID just stay the same
                state.addAndDistributeUnreducibilityInfo(ucTask.ucID, ucTask.candidate);
                //we learn that the current selector variables cannot be 1 together
                List<Integer> metaClause = new LinkedList<Integer>();
                int numClauses = state.getStatistics().numClausesOrGroups;
                for (int i = 1; i <= numClauses; i++)
                {
                    if (!result.getUc().contains(i) || i == ucTask.reductionID)
                    {
                        metaClause.add(-i);
                    }
                }
                state.learnMetaClause(metaClause);
            }
            //attempt was successful, we might have arrived at a new UC
            else
            {
                int stepID = state.registerMUC(result, ucTask.ucID, ucTask.candidate);
                ucTask.uc.setRemovalLink(ucTask.candidate, stepID);
                MUCStep uc = state.retrieve(MUCStep.class, stepID);
                if (kahina.getState().usesMetaLearning())
                {
                    List<Integer> posSelVars = new LinkedList<Integer>();
                    int numClauses = state.getStatistics().numClausesOrGroups;
                    for (int i = 1; i <= numClauses; i++)
                    {
                        if (!uc.getUc().contains(i))
                        {
                            posSelVars.add(i);
                        }
                    }
                    List<Integer> learnedUnits = MiniSAT.getImpliedUnits(state.getMetaInstance(), posSelVars);
                    //System.err.println("Learned Units: " + learnedUnits);
                    for (int learnedUnit : learnedUnits)
                    {
                        //TODO: extend this to positive units as soon as we can learn some!
                        if (learnedUnit < 0)
                        {
                            System.err.println("Unit from meta problem: " + learnedUnit);
                            uc.setRemovalLink(-learnedUnit, -1);
                        }
                    }
                }
            }
            //TODO: panel.updateLabelColors(ucTask.uc);
            kahina.dispatchEvent(new KahinaUpdateEvent(kahina.getState().getSelectedStepID()));
            kahina.dispatchEvent(new KahinaRedrawEvent());
        }
    }
}
