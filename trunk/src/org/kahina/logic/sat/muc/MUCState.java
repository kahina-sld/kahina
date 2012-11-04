package org.kahina.logic.sat.muc;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import org.kahina.core.KahinaState;
import org.kahina.core.control.KahinaController;
import org.kahina.core.data.dag.ColoredPathDAG;
import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.data.model.CompleteAssignment;
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.logic.sat.io.minisat.MiniSATFiles;
import org.kahina.logic.sat.muc.bridge.MUCInstruction;
import org.kahina.logic.sat.muc.data.MUCMetaInstance;
import org.kahina.logic.sat.muc.data.MUCStatistics;
import org.kahina.logic.sat.muc.data.PartitionBlockHandler;
import org.kahina.logic.sat.muc.data.RecursiveBlockHandler;
import org.kahina.logic.sat.muc.data.UCReducerList;

public class MUCState extends KahinaState
{
    public static boolean VERBOSE = false;
    
    CnfSatInstance satInstance;
    MUCStatistics stat;
    MiniSATFiles files;
    
    MUCMetaInstance metaInstance;
    PartitionBlockHandler partitionBlocks;
    RecursiveBlockHandler recursiveBlocks;
    
    ColoredPathDAG decisionGraph;
    UCReducerList reducers;
    
    Map<MUCStep,Integer> nodeForStep;
    
    MUCInstance kahina;
    
    public MUCState(MUCInstance kahina)
    {
        super(kahina);
        this.kahina = kahina;
        this.satInstance = null;
        this.metaInstance = null;
        this.partitionBlocks = null;
        this.recursiveBlocks = null;
        this.stat = null;
        this.files = null;
        this.nodeForStep = new HashMap<MUCStep,Integer>();
    }
    
    public MUCState(MUCInstance kahina, CnfSatInstance satInstance, MUCStatistics stat, MiniSATFiles files)
    {
        super(kahina);
        this.kahina = kahina;
        this.satInstance = satInstance;
        if (usesMetaLearning())
        {
            this.metaInstance = new MUCMetaInstance(satInstance.getNumClauses());
        }
        if (kahina.getMetaLearningMode() == MetaLearningMode.BLOCK_PARTITION)
        {
            this.partitionBlocks = new PartitionBlockHandler(metaInstance);
            this.metaInstance.setBlockHandler(partitionBlocks);
        }
        if (kahina.getMetaLearningMode() == MetaLearningMode.RECURSIVE_BLOCKS)
        {
            this.recursiveBlocks = new RecursiveBlockHandler(metaInstance);
            this.metaInstance.setBlockHandler(recursiveBlocks);
        }
        this.stat = stat;
        this.files = files;
        this.nodeForStep = new HashMap<MUCStep,Integer>();
    }
    
    public void initialize()
    {
        super.initialize();
        decisionGraph = new ColoredPathDAG();
        reducers = new UCReducerList();
    }
    
    public void reset()
    {
        this.satInstance = null;
        this.metaInstance = null;
        this.partitionBlocks = null;
        this.recursiveBlocks = null;
        this.stat = null;
        this.files = null;
        this.nodeForStep = new HashMap<MUCStep,Integer>();
        initialize();
    }
    
    public boolean usesMetaLearning()
    {
        return kahina.getMetaLearningMode() != MetaLearningMode.NO_META_LEARNING;
    }
    
    public MUCStep getSelectedStep()
    {
        int stepID = getSelectedStepID();
        if (stepID == -1) return null;
        return retrieve(MUCStep.class, stepID);
    }
    
    public CnfSatInstance getSatInstance()
    {
        return satInstance;
    }
    
    public MUCMetaInstance getMetaInstance()
    {
        return metaInstance;
    }
    
    public MUCStatistics getStatistics()
    {
        return stat;
    }
    
    public MiniSATFiles getFiles()
    {
        return files;
    }
    
    public ColoredPathDAG getDecisionGraph()
    {
        return decisionGraph;
    }
    
    public UCReducerList getReducers()
    {
        return reducers;
    }
    
    public PartitionBlockHandler getPartitionBlocks()
    {
        return partitionBlocks;
    }
    
    public RecursiveBlockHandler getRecursiveBlocks()
    {
        return recursiveBlocks;
    }
    
    public List<TreeSet<Integer>> getBlocksForUC(int ucID)
    {
        List<TreeSet<Integer>> blocks = new LinkedList<TreeSet<Integer>>();
        MUCStep ucStep = retrieve(MUCStep.class, ucID);
        //go through blocks and see whether they are contained in the UC
        for (TreeSet<Integer> block : metaInstance.getBlocks())
        {
            boolean blockContained = true;
            //in the block definitions, literals are negative
            for (int negLit : block)
            {
                if (!ucStep.getUc().contains(-negLit))
                {
                    blockContained = false;
                    break;
                }
            }
            if (blockContained)
            {
                blocks.add(block);
            }
        }
        return blocks;
    }
    
    public synchronized int registerMUC(MUCStep newStep, int parentID, List<Integer> selCandidates)
    {
        Integer stepID = nodeForStep.get(newStep);
        if (stepID == null)
        {
            stepID = nextStepID();
            store(stepID, newStep);
            nodeForStep.put(newStep, stepID);
            
            if (VERBOSE) System.err.println("Adding step with ID " + stepID + " to decision graph!");
            
            //initialize: add the root node
            if (parentID == -1)
            {
                decisionGraph.addNode(stepID, "Init: " + newStep.getUc().size() + "", MUCStepType.ACTIVE);
            }
            else
            {
                decisionGraph.addNode(stepID, newStep.getUc().size() + "", MUCStepType.ACTIVE);
                if (VERBOSE) System.err.println("Adding decision graph edge (" + parentID + "," + stepID + ")");
                decisionGraph.addEdgeNoDuplicates(parentID, stepID, selCandidates + "");
                for (int selCandidate : selCandidates)
                {
                    retrieve(MUCStep.class, parentID).setRemovalLink(selCandidate, stepID);
                }
                propagateReducibilityInfo(parentID, stepID);
            }
        }
        else
        {
            if (VERBOSE) System.err.println("Adding decision graph edge (" + parentID + "," + stepID + ")");
            decisionGraph.addEdgeNoDuplicates(parentID, stepID, selCandidates + "");
            for (int selCandidate : selCandidates)
            {
                retrieve(MUCStep.class, parentID).setRemovalLink(selCandidate, stepID);
            }
            propagateReducibilityInfo(parentID, stepID);
        }
        if (VERBOSE)
        {
            System.err.println("State of the nodeForStep map:");
            for (MUCStep step : nodeForStep.keySet())
            {
                System.err.println(nodeForStep.get(step) + ": " + step.getUc());
            }
        } 
        return stepID;
    }
    
    public synchronized int registerMUC(Integer[] mucCandidates, Integer[] muc, MUCInstruction lastInstruction, int parentID)
    {
        MUCStep newStep = new MUCStep();
        List<Integer> uc = newStep.getUc();
        for (int i = 0; i < mucCandidates.length; i++)
        {
            uc.add(mucCandidates[i]);
            //icStatus = 0 (default)
        }
        for (int i = 0; i < muc.length; i++)
        {
            uc.add(muc[i]);
            newStep.setRemovalLink(muc[i], -1);
        }
        uc.remove((Object) 0);
        
        Integer stepID = nodeForStep.get(newStep);
        if (stepID == null)
        {
            stepID = nextStepID();
            store(stepID, newStep);
            nodeForStep.put(newStep, stepID);
            
            System.err.println("Adding step with ID " + stepID + " to decision graph!");
            
            //initialize: add the root node
            if (lastInstruction == null)
            {
                decisionGraph.addNode(stepID, "Init: " + uc.size() + "", MUCStepType.ACTIVE);
            }
            else
            {
                decisionGraph.addNode(stepID, uc.size() + "", MUCStepType.ACTIVE);
                if (VERBOSE) System.err.println("Adding decision graph edge (" + parentID + "," + stepID + ")");
                decisionGraph.addEdgeNoDuplicates(parentID, stepID, lastInstruction.selCandidate + "");
                if (mucCandidates.length == 0)
                {
                    decisionGraph.setNodeStatus(stepID, MUCStepType.MINIMAL);
                }
                retrieve(MUCStep.class, parentID).setRemovalLink(lastInstruction.selCandidate, stepID);
                if (!usesMetaLearning()) propagateReducibilityInfo(parentID, stepID);
            }
        }
        else
        {
            if (VERBOSE) System.err.println("Adding decision graph edge (" + parentID + "," + stepID + ")");
            decisionGraph.addEdgeNoDuplicates(parentID, stepID, lastInstruction.selCandidate + "");
            if (mucCandidates.length == 0)
            {
                decisionGraph.setNodeStatus(stepID, MUCStepType.MINIMAL);
            }
            retrieve(MUCStep.class, parentID).setRemovalLink(lastInstruction.selCandidate, stepID);
            if (!usesMetaLearning()) propagateReducibilityInfo(parentID, stepID);
        }
        if (VERBOSE)
        {
            System.err.println("State of the nodeForStep map:");
            for (MUCStep step : nodeForStep.keySet())
            {
                System.err.println(step.getUc() + "->" + nodeForStep.get(step));
            }
        }  
        return stepID;
    }

    public void setSatInstance(CnfSatInstance satInstance)
    {
        this.satInstance = satInstance;
        if (kahina.getMetaLearningMode() != MetaLearningMode.NO_META_LEARNING)
        {
            this.metaInstance = new MUCMetaInstance(satInstance.getNumClauses());
        }
        if (kahina.getMetaLearningMode() == MetaLearningMode.BLOCK_PARTITION)
        {
            this.partitionBlocks = new PartitionBlockHandler(metaInstance);
            this.metaInstance.setBlockHandler(partitionBlocks);
        }
        if (kahina.getMetaLearningMode() == MetaLearningMode.RECURSIVE_BLOCKS)
        {
            this.recursiveBlocks = new RecursiveBlockHandler(metaInstance);
            this.metaInstance.setBlockHandler(recursiveBlocks);
        }
    }
    
    public void setStatistics(MUCStatistics stat)
    {
        this.stat = stat;
    }
    
    public void setFiles(MiniSATFiles files)
    {
        this.files = files;
    }
    
    protected void processSelection()
    {
        if (usesMetaLearning() && getSelectedStep() != null)
        {
            learnMetaUnits(getSelectedStep());
            /*System.err.println("blocks for UC " + getSelectedStepID() + ":");
            for (List<Integer> block : getBlocksForUC(getSelectedStepID()))
            {
                System.err.println("  " + block);
            }*/
        }
    }
    
    public synchronized void modelRotation(CompleteAssignment model, int ucID, int transClID)
    {
        if (VERBOSE) System.err.println("modelRotation(model," + ucID + "," + transClID + ")");
        for (int literal : satInstance.getClause(transClID))
        {
            int var = Math.abs(literal);
            model.flipVar(var);
            if (VERBOSE) System.err.println("  modifying model by " + var + " := " + model.getValue(var));
            int singleTransClForFlippedModel = -1;
            MUCStep uc = retrieve(MUCStep.class, ucID);
            for (int ic : uc.getUc())
            {
                if (uc.getIcStatus(ic) == 0 && !model.satisfies(satInstance.getClause(ic)))
                {
                    if (VERBOSE) System.err.println("  model does not satisfy clause " + ic);
                    if (singleTransClForFlippedModel == -1)
                    {
                        singleTransClForFlippedModel = ic;
                    }
                    else
                    {
                        singleTransClForFlippedModel = -1;
                        if (VERBOSE) System.err.println("  no unique transition clause, aborted!");
                        break;
                    }
                }
            }
            if (singleTransClForFlippedModel != -1)
            {
               if (VERBOSE) System.err.println("Meta learning in node " + ucID + ": " + singleTransClForFlippedModel + " is part of MUC!");
               //the transition clause associated with the new model must be part of the MUS
               uc.setRemovalLink(singleTransClForFlippedModel, -1);
               if (!usesMetaLearning())
               {
                   addAndDistributeUnreducibilityInfo(ucID, singleTransClForFlippedModel);
               }
               else
               {
                   //we learn that the current selector variables cannot be 1 together
                   TreeSet<Integer> metaClause = new TreeSet<Integer>();
                   int numClauses = getStatistics().numClausesOrGroups;
                   for (int i = 1; i <= numClauses; i++)
                   {
                       if (!uc.getUc().contains(i) || i == singleTransClForFlippedModel)
                       {
                           metaClause.add(-i);
                       }
                   }
                   learnMetaClause(metaClause);
               }
               //recursive case: continue with the modified model
               modelRotation(model,ucID,singleTransClForFlippedModel);
            }
            else
            {
                model.flipVar(var);
            }
        }
    }
    
    public synchronized void learnMetaBlock(TreeSet<Integer> metaBlock)
    {
        long time = System.currentTimeMillis();
        metaInstance.learnNewBlock(metaBlock);
        System.err.println("  " + (System.currentTimeMillis() - time) + " ms for learning meta block.");
    }
    
    public synchronized void learnMetaClause(TreeSet<Integer> metaClause)
    {
        long time = System.currentTimeMillis();
        metaInstance.learnNewClause(metaClause);
        System.err.println("  " + (System.currentTimeMillis() - time) + " ms for learning meta clause.");
    }
    
    public synchronized void learnMetaUnits(MUCStep uc)
    {
        long time = System.currentTimeMillis();
        if (VERBOSE) System.err.print("Learning meta units: ");
        List<Integer> posSelVars = new LinkedList<Integer>();
        int numClauses = getStatistics().numClausesOrGroups;
        for (int i = 1; i <= numClauses; i++)
        {
            if (!uc.getUc().contains(i))
            {
                posSelVars.add(i);
            }
        }
        //List<Integer> learnedUnits = MiniSAT.getImpliedUnits(getMetaInstance(), posSelVars);
        List<Integer> learnedUnits = getMetaInstance().deriveUnitsByPropagation(posSelVars);
        //System.err.println("Learned Units: " + learnedUnits);
        for (int learnedUnit : learnedUnits)
        {
            //TODO: extend this to positive units as soon as we can learn them!
            if (learnedUnit < 0)
            {
                if (VERBOSE) System.err.print(learnedUnit + " ");
                uc.setRemovalLink(-learnedUnit, -1);
            }
        }
        if (VERBOSE) System.err.println();
        System.err.println("  " + (System.currentTimeMillis() - time) + " ms for learning meta units");
    }
    
    public synchronized void addAndDistributeUnreducibilityInfo(int parentID, int failedReductionID)
    {
        MUCStep parentStep = retrieve(MUCStep.class, parentID);
        parentStep.setRemovalLink(failedReductionID, -1);
        for (int cand : parentStep.getUc())
        {
            Integer link = parentStep.getRemovalLink(cand);
            if (link != null && link != -1)
            {
                addAndDistributeUnreducibilityInfo(link, failedReductionID);
            }
        }
    }
    
    public synchronized void propagateReducibilityInfo(int parentID, int childID)
    {
        MUCStep parentStep = retrieve(MUCStep.class, parentID);
        MUCStep childStep = retrieve(MUCStep.class, childID);
        for (int cand : parentStep.getUc())
        {
            Integer link = parentStep.getRemovalLink(cand);
            if (link != null && parentStep.getRemovalLink(cand) == -1)
            {
                childStep.setRemovalLink(cand, -1);
            }
        }
        for (int cand : childStep.getUc())
        {
            Integer link = childStep.getRemovalLink(cand);
            if (link != null && link != -1)
            {
                propagateReducibilityInfo(childID, link);
            }
        }
    }
}
