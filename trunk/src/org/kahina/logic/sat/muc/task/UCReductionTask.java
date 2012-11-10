package org.kahina.logic.sat.muc.task;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.List;
import java.util.concurrent.TimeoutException;

import org.kahina.core.gui.KahinaProgressBar;
import org.kahina.core.task.KahinaTask;
import org.kahina.core.task.KahinaTaskManager;
import org.kahina.logic.sat.data.model.CompleteAssignment;
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.logic.sat.io.minisat.MiniSATFiles;
import org.kahina.logic.sat.muc.MUCStep;
import org.kahina.logic.sat.muc.data.MUCStatistics;

public class UCReductionTask extends KahinaTask
{
    //every UCReductionTask receives a unique numerical ID upon creation; used for file names
    public static int idCounter = 0;
    
    //determines if model rotation is applied after an unsuccessful reduction
    private boolean modelRotation = false;
    
    public final int reductionID;
    
    public final MUCStep uc;
    public final int ucID;
    public final List<Integer> candidates;
    
    private MUCStep result;
    private CompleteAssignment model;
    
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
    public UCReductionTask(KahinaProgressBar progressBar, KahinaTaskManager manager, MUCStatistics stat, MUCStep uc, int ucID, List<Integer> candidates, MUCStep result)
    {
        super(progressBar, manager);
        //this.reducer = reducer;
        this.stat = stat;
        reductionID = getNextID();
        this.uc = uc;
        this.ucID = ucID;
        this.candidates = candidates;
        this.result = result;
        this.model = null;
    }

    public UCReductionTask(KahinaProgressBar progressBar, KahinaTaskManager manager, MUCStatistics stat,  MUCStep uc, int ucID, List<Integer> candidates, MiniSATFiles files)
    {
        super(progressBar, manager);
        //this.reducer = reducer;
        this.stat = stat;
        reductionID = getNextID();
        this.uc = uc;
        this.ucID = ucID;
        this.candidates = candidates;
        this.files = files.copyWithoutTmpFiles();
        this.model = null;
    }
    
    public static synchronized int getNextID()
    {
        return idCounter++;
    }

    public boolean usesModelRotation()
    {
        return modelRotation;
    }

    public void setModelRotation(boolean modelRotation)
    {
        this.modelRotation = modelRotation;
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
            for (int candidate : candidates)
            {
                //wrap in Integer object in order to remove the element candidate, not at the index candidate
                muc_cands.remove(new Integer(candidate));
            }
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
                if (modelRotation)
                {
                    model = MiniSAT.getCompleteModel(files.tmpResultFile);
                }
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
                if (candidates.size() == 1)
                {
                    uc.setRemovalLink(candidates.get(0), -1);
                }
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
     * 
     * @return the model in case of a SAT result, if modelRotation is set; null otherwise
     */
    public CompleteAssignment getModel()
    {
        return model;
    }
    
    /**
     * Retrieve the MUCStep resulting from the reduction (attempt)
     * @return a new MUCStep if the reduction was successful, otherwise the input step with an additional clause marked as irreducible
     */
    public MUCStep getResult()
    {
        return result;
    }
    
    public String toString()
    {
        return "\"reduction #" + reductionID + ": reduce in uc " + ucID + " by " + candidates + "\""; 
    }
}
