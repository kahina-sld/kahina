package org.kahina.logic.sat.muc.task;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeoutException;

import org.kahina.core.gui.KahinaProgressBar;
import org.kahina.core.task.KahinaTask;
import org.kahina.core.task.KahinaTaskManager;
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.logic.sat.io.minisat.MiniSATFiles;
import org.kahina.logic.sat.muc.MUCStep;
import org.kahina.logic.sat.muc.data.MUCStatistics;

public class UCReductionTask extends KahinaTask
{
    //every UCReductionTask receives a unique numerical ID upon creation; used for file names
    public static int idCounter = 0;
    
    public final int reductionID;
    
    public final MUCStep uc;
    public final int ucID;
    public final int candidate;
    
    MUCStep result;
    
    MUCStatistics stat;
    
    MiniSATFiles files;
    
    /**
     * Construct a pseudo-reduction task for a known outcome which simply retrieves a result step.
     * @param progressBar
     * @param reducer
     * @param uc
     * @param ucID
     * @param candidate
     */
    public UCReductionTask(KahinaProgressBar progressBar, KahinaTaskManager manager, MUCStatistics stat, MUCStep uc, int ucID, int candidate, MUCStep result)
    {
        super(progressBar, manager);
        //this.reducer = reducer;
        this.stat = stat;
        reductionID = getNextID();
        this.uc = uc;
        this.ucID = ucID;
        this.candidate = candidate;
        this.result = result;
    }

    public UCReductionTask(KahinaProgressBar progressBar, KahinaTaskManager manager, MUCStatistics stat,  MUCStep uc, int ucID, int candidate, MiniSATFiles files)
    {
        super(progressBar, manager);
        //this.reducer = reducer;
        this.stat = stat;
        reductionID = getNextID();
        this.uc = uc;
        this.ucID = ucID;
        this.candidate = candidate;
        this.files = files.copyWithoutTmpFiles();
    }
    
    public static synchronized int getNextID()
    {
        return idCounter++;
    }

    @Override
    public void run()
    {    
        if (result == null)
        {
            files.createTempFiles(files.sourceFile.getName() + reductionID);
            //set the freeze variables (TODO: avoid generating the different lists first)
            List<Integer> muc_cands = new ArrayList<Integer>();
            //List<Integer> muc = new ArrayList<Integer>();
            for (int i : uc.getUc())
            {
                muc_cands.add(i);
            }
            //wrap in Integer object in order to remove the element candidate, not at the index candidate
            muc_cands.remove(new Integer(candidate));
            boolean[] freezeVariables = new boolean[stat.numClausesOrGroups];
            Arrays.fill(freezeVariables, Boolean.FALSE);
            for (Integer a : muc_cands)
            {
                freezeVariables[a] = true;
            }
            MiniSAT.createFreezeFile(freezeVariables, files.tmpFreezeFile, stat.highestID + 1);
            List<Integer> reducedCore = null;
            try
            {
                reducedCore = MiniSAT.findUnsatisfiableCore(stat, files);
            }
            catch (InterruptedException e)
            {
                System.err.println("ERROR: InterruptedException while executing UC reduction task!");
                result = null;
                return;
            }
            catch (TimeoutException e)
            {
                System.err.println("ERROR: TimeoutException while executing UC reduction task!");
                result = null;
                return;
            }
            //reduction attempt failed, the result was satisfiable
            if (reducedCore.size() == 0)
            {
                result = uc;
                uc.setRemovalLink(candidate, -1);
            }
            //reduction attempt was successful
            else
            {
                MUCStep newStep = new MUCStep();
                List<Integer> uc = newStep.getUc();
                for (Integer a : reducedCore)
                {
                    //if (!muc.contains(a))
                    {
                        //icStatus = 0 (default)
                        uc.add(a);
                    }
                }
                /*for (int i = 0; i < muc.size(); i++)
                {
                    uc.add(muc.get(i));
                    newStep.setIcStatus(muc.get(i), 2);
                }*/
                uc.remove(new Integer(0));
                result = newStep;
            }  
            //delete temporary files
            files.deleteTempFiles();
        }
        //for dummy reduction tasks where the result was known before
        else
        {
            //no IC status needs to be set because we knew the result before
        }
        this.setFinished();
    }
    
    /**
     * Retrieve the MUCStep resulting from the reduction (attempt)
     * @return a new MUCStep if the reduction was successful, otherwise the input step with an additional clause marked as irreducible
     */
    public MUCStep getResult()
    {
        return result;
    }
}
