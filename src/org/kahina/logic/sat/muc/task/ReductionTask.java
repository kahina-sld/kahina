package org.kahina.logic.sat.muc.task;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.List;
import java.util.TreeSet;
import java.util.concurrent.TimeoutException;

import org.kahina.core.gui.KahinaProgressBar;
import org.kahina.core.task.KahinaTask;
import org.kahina.core.task.KahinaTaskManager;
import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.data.model.CompleteAssignment;
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.logic.sat.io.minisat.MiniSATFiles;
import org.kahina.logic.sat.muc.MUCStep;
import org.kahina.logic.sat.muc.data.MUCStatistics;

public class ReductionTask extends KahinaTask
{
    //every UCReductionTask receives a unique numerical ID upon creation; used for file names
    public static int idCounter = 0;
    
    //determines if clause set refinement is applied after a successful reduction
    private boolean clauseSetRefinement = true;
    //determines if model rotation is applied after an unsuccessful reduction
    private boolean modelRotation = false;
    //determines if autarky reduction is applied after a successful reduction
    private boolean autarkyReduction = false;
    
    public final int reductionID;
    
    public final MUCStep uc;
    public final int ucID;
    public final List<Integer> candidates;
    
    //remember whether this reduction task was just simulated
    private final boolean dummyTask;
    
    private MUCStep result;
    private CompleteAssignment model;
    
    MUCStatistics stat;
    
    MiniSATFiles files;
    
    CnfSatInstance instance;
    
    /**
     * Construct a pseudo-reduction task for a known outcome which simply retrieves a result step.
     * @param progressBar
     * @param reducer
     * @param uc
     * @param ucID
     * @param candidate
     */
    public ReductionTask(KahinaProgressBar progressBar, KahinaTaskManager manager, MUCStatistics stat, MUCStep uc, int ucID, List<Integer> candidates, MUCStep result, CnfSatInstance instance)
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
        this.instance = instance;
        this.dummyTask = true;
    }

    public ReductionTask(KahinaProgressBar progressBar, KahinaTaskManager manager, MUCStatistics stat,  MUCStep uc, int ucID, List<Integer> candidates, MiniSATFiles files, CnfSatInstance instance)
    {
        super(progressBar, manager);
        //this.reducer = reducer;
        this.stat = stat;
        reductionID = getNextID();
        this.uc = uc;
        this.ucID = ucID;
        this.candidates = candidates;
        //the file object is needed for the ReductionAgent to find the proof file
        this.files = files;
        this.result = null;
        this.model = null;
        this.instance = instance;
        this.dummyTask = false;
    }
    
    public static synchronized int getNextID()
    {
        return idCounter++;
    }
    
    public boolean usesClauseSetRefinement()
    {
        return clauseSetRefinement;
    }
    
    public void setClauseSetRefinement(boolean clauseSetRefinement)
    {
        this.clauseSetRefinement = clauseSetRefinement;
    }

    public boolean usesModelRotation()
    {
        return modelRotation;
    }

    public void setModelRotation(boolean modelRotation)
    {
        this.modelRotation = modelRotation;
    }
    
    public boolean usesAutarkyReduction()
    {
        return autarkyReduction;
    }
    
    public void setAutarkyReduction(boolean autarkyReduction)
    {
        this.autarkyReduction = autarkyReduction;
    }
    
    public boolean isDummyTask()
    {
        return dummyTask;
    }

    @Override
    public void run()
    {    
        if (result == null)
        {
            //System.err.println("files.createTempFiles(" + files.sourceFile.getName() + reductionID + ");");
            files.createTempFiles(files.sourceFile.getName() + reductionID);
            //set the freeze variables (TODO: avoid generating the different lists first)
            TreeSet<Integer> muc_cands = new TreeSet<Integer>();
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
            int[] freezeVariables = new int[stat.numClausesOrGroups];
            Arrays.fill(freezeVariables, -1);
            for (int i = 1; i <= stat.numClausesOrGroups; i++)
            {
                if (instance.isDontCareClause(i) || muc_cands.contains(i))
                {
                    freezeVariables[i-1] = 1;
                }
            }
            //System.err.println("freezeVars: " + Arrays.toString(freezeVariables));
            MiniSAT.createFreezeFile(freezeVariables, files.tmpFreezeFile, stat.highestID + 1);
            List<Integer> reducedCore = null;
            try
            {
                if (clauseSetRefinement)
                {
                    reducedCore = MiniSAT.findUnsatisfiableCore(stat, files);
                }
                else
                {
                    //without clause set refinement, the tested US candidate is the result of a successful reduction attempt
                    MiniSAT.solve(files.tmpFile, files.tmpProofFile, files.tmpResultFile, files.tmpFreezeFile);
                    if (MiniSAT.wasUnsatisfiable(files.tmpResultFile))
                    {
                        reducedCore = new ArrayList<Integer>(muc_cands);
                    }
                    else
                    {
                        //empty core signals satisfiability, i.e. an unsuccessful reduction attempt
                        reducedCore = new ArrayList<Integer>();
                    }
                }
                System.err.println("reducedCore: " + reducedCore);
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
                //we only receive criticality information if there was a single reduction candidate
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
                    if (!instance.isDontCareClause(a))
                    {
                        uc.add(a);
                    }
                }
                /*for (int i = 0; i < muc.size(); i++)
                {
                    uc.add(muc.get(i));
                    newStep.setIcStatus(muc.get(i), 2);
                }*/
                //TODO: apply autarky reduction to the new uc if so defined
                uc.remove(new Integer(0));
                result = newStep;
            }  
        }
        //for simulated reduction tasks where the result was known before
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
