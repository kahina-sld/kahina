package org.kahina.logic.sat.insertionmus;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.kahina.core.KahinaState;
import org.kahina.core.control.KahinaController;
import org.kahina.core.data.dag.ColoredPathDAG;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.data.model.CompleteAssignment;
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.logic.sat.io.minisat.MiniSATFiles;
import org.kahina.logic.sat.muc.MUCStepType;
import org.kahina.logic.sat.muc.bridge.MUCInstruction;
import org.kahina.logic.sat.muc.data.BlocklessBlockHandler;
import org.kahina.logic.sat.muc.data.MUCMetaInstance;
import org.kahina.logic.sat.muc.data.MUCStatistics;
import org.kahina.logic.sat.muc.data.Overlap;
import org.kahina.logic.sat.muc.data.PartitionBlockHandler;
import org.kahina.logic.sat.muc.data.RecursiveBlockHandler;
import org.kahina.logic.sat.muc.data.UCReducerList;

public class MUCState extends KahinaState
{
    public static boolean VERBOSE = false;
    
    CnfSatInstance satInstance;
//    MUCStatistics stat;
    MiniSATFiles files;
    
    MUCMetaInstance metaInstance;
    BlocklessBlockHandler blocklessBlocks;
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
        this.blocklessBlocks = null;
        this.partitionBlocks = null;
        this.recursiveBlocks = null;
//        this.stat = null;
        this.files = null;
        this.nodeForStep = new HashMap<MUCStep,Integer>();
    }
    
    public MUCState(MUCInstance kahina, CnfSatInstance satInstance, MiniSATFiles files)
    {
        super(kahina);
        this.kahina = kahina;
        this.satInstance = satInstance;


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
        this.blocklessBlocks = null;  
        this.partitionBlocks = null;
        this.recursiveBlocks = null;
//        this.stat = null;
        this.files = null;
        this.nodeForStep = new HashMap<MUCStep,Integer>();
        initialize();
    }
    
    public MUCInstance getKahina()
    {
        return kahina;
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
    
//    public MUCStatistics getStatistics()
//    {
//        return stat;
//    }
//    
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
    
    public BlocklessBlockHandler getBlocklessBlocks()
    {
        return blocklessBlocks;
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
                decisionGraph.addNode(stepID, "Init: " + newStep.getUc().size() + "", MUCStepType.UNKNOWN);
            }
            else
            {
                decisionGraph.addNode(stepID, newStep.getUc().size() + "", MUCStepType.UNKNOWN);
                if (VERBOSE) System.err.println("Adding decision graph edge (" + parentID + "," + stepID + ")");
                decisionGraph.addEdgeNoDuplicates(parentID, stepID, selCandidates + "");
                for (int selCandidate : selCandidates)
                {
                    retrieve(MUCStep.class, parentID).setRemovalLink(selCandidate, stepID);
                }
                propagateIrreducibilityInfo(parentID, stepID);
                updateDecisionNode(stepID);
            }
        }
        else if (stepID != parentID)
        {
            if (VERBOSE) System.err.println("Adding decision graph edge (" + parentID + "," + stepID + ")");
            decisionGraph.addEdgeNoDuplicates(parentID, stepID, selCandidates + "");
            for (int selCandidate : selCandidates)
            {
                retrieve(MUCStep.class, parentID).setRemovalLink(selCandidate, stepID);
            }
            propagateIrreducibilityInfo(parentID, stepID);
            updateDecisionNode(stepID);
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
            if (!satInstance.isDontCareClause(mucCandidates[i]))
            {
                uc.add(mucCandidates[i]);
                //icStatus = 0 (default)
            }
        }
        for (int i = 0; i < muc.length; i++)
        {
            if (!satInstance.isDontCareClause(muc[i]))
            {
                uc.add(muc[i]);
                newStep.setRemovalLink(muc[i], -1);
            }
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
                decisionGraph.addNode(stepID, "Init: " + uc.size() + "", MUCStepType.UNKNOWN);
            }
            else
            {
                decisionGraph.addNode(stepID, uc.size() + "", MUCStepType.UNKNOWN);
                if (VERBOSE) System.err.println("Adding decision graph edge (" + parentID + "," + stepID + ")");
                decisionGraph.addEdgeNoDuplicates(parentID, stepID, lastInstruction.selCandidate + "");
                if (mucCandidates.length == 0)
                {
                    decisionGraph.setNodeStatus(stepID, MUCStepType.MINIMAL);
                }
                retrieve(MUCStep.class, parentID).setRemovalLink(lastInstruction.selCandidate, stepID);
                propagateIrreducibilityInfo(parentID, stepID);
                updateDecisionNode(stepID);
            }
        }
        else if (stepID != parentID)
        {
            if (VERBOSE) System.err.println("Adding decision graph edge (" + parentID + "," + stepID + ")");
            decisionGraph.addEdgeNoDuplicates(parentID, stepID, lastInstruction.selCandidate + "");
            if (mucCandidates.length == 0)
            {
                decisionGraph.setNodeStatus(stepID, MUCStepType.MINIMAL);
            }
            retrieve(MUCStep.class, parentID).setRemovalLink(lastInstruction.selCandidate, stepID);
            propagateReducibilityInfo(stepID, parentID);
            propagateIrreducibilityInfo(parentID, stepID);
            updateDecisionNode(stepID);
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

    }
    
//    public void setStatistics(MUCStatistics stat)
//    {
//        this.stat = stat;
//    }
    
    public void setFiles(MiniSATFiles files)
    {
        this.files = files;
    }
    
//    protected void processSelection()
//    {
//        if (usesMetaLearning() && getSelectedStep() != null)
//        {
//            learnMetaUnits(getSelectedStep());
//            /*System.err.println("blocks for UC " + getSelectedStepID() + ":");
//            for (List<Integer> block : getBlocksForUC(getSelectedStepID()))
//            {
//                System.err.println("  " + block);
//            }*/
//        }
//    }
    
    public int autarkyReduction(int ucID)
    {
        //extract the lean kernel from the currently selected US
        MUCStep uc = retrieve(MUCStep.class, ucID);

        CnfSatInstance leanKernelUC = kahina.getSatInstance().selectClauses(uc.getUc()).copy();
        leanKernelUC.reduceToLeanKernel();
        
        //generate a new US representing the lean kernel, also detecting duplicates 
        MUCStep leanUc = new MUCStep();
        List<Integer> leanUS = leanUc.getUc();
        Set<Integer> leanUSSet = new HashSet<Integer>();
        Map<String,Integer> idMap = kahina.getSatInstance().generateClauseToIndexMap();
        StringBuilder clauseRepresentation;
        for (int i = 0; i < leanKernelUC.getSize(); i++)
        {
            clauseRepresentation = new StringBuilder();
            for (Integer lit : leanKernelUC.getClause(i))
            {
                clauseRepresentation.append(lit + ".");
            }
            int a = idMap.get(clauseRepresentation.toString());
            if (!leanUSSet.contains(a+1) && !kahina.getSatInstance().isDontCareClause(a))
            {
                leanUS.add(a+1);
                leanUSSet.add(a+1);
            }
        }
        
        if (leanUS.size() != uc.getUc().size())
        {
            //add a node with the lean kernel US to the reduction graph
            int resultID = registerMUC(leanUc, ucID, new LinkedList<Integer>());
            Overlap overlap = new Overlap(uc.getUc(),leanUc.getUc());
            //System.err.println(overlap);
            for (int candidate : overlap.aMinusB)
            {
                if (uc.getRemovalLink(candidate) == null)
                {
                    addAndDistributeReducibilityInfo(ucID, candidate, -2);
                }
            }
            updateDecisionNode(ucID);
            System.err.println("  autarky reduction in US " + ucID + " caused " + overlap.aMinusB.size() + " clauses to fall away.");
            return resultID;
        }
        else
        {
            System.err.println("  autarky reduction in US " + ucID + " caused no clauses to fall away.");
            return ucID;
        }
    }
    
    
//    public synchronized void learnMetaBlock(TreeSet<Integer> metaBlock)
//    {
//        kahina.getLogger().startMeasuring();
//        metaInstance.learnNewBlock(metaBlock);
//        kahina.getLogger().endMeasuring("for learning meta block.");
//    }
//    
//    public synchronized void learnMetaClause(TreeSet<Integer> metaClause)
//    {
//        kahina.getLogger().startMeasuring();
//        metaInstance.learnNewClause(metaClause);
//        kahina.getLogger().endMeasuring("for learning meta clause.");
//    }
//    
//    public synchronized void learnMetaUnits(MUCStep uc)
//    {
//        kahina.getLogger().startMeasuring();
//        List<Integer> posSelVars = new LinkedList<Integer>();
////        int numClauses = getStatistics().numClausesOrGroups;
////        for (int i = 1; i <= numClauses; i++)
////        {
////            if (!uc.getUc().contains(i))
////            {
////                posSelVars.add(i);
////            }
////        }
//        //List<Integer> learnedUnits = MiniSAT.getImpliedUnits(getMetaInstance(), posSelVars);
//        List<Integer> learnedUnits = getMetaInstance().deriveUnitsByPropagation(posSelVars);
//        //System.err.println("Learned Units: " + learnedUnits);
//        for (int learnedUnit : learnedUnits)
//        {
//            //TODO: extend this to negative units as soon as we can learn them!
//            if (learnedUnit > 0  && uc.getUc().contains(learnedUnit))
//            {
//                if (VERBOSE) System.err.print(learnedUnit + " ");
//                uc.setRemovalLink(learnedUnit, -1);
//            }
//        }
//        if (VERBOSE) System.err.println();
//        kahina.getLogger().endMeasuring("for learning meta units");
//    }
    
//    public synchronized void addAndDistributeIrreducibilityInfo(int parentID, int failedReductionID)
//    {
//        MUCStep parentStep = retrieve(MUCStep.class, parentID);
//        parentStep.setRemovalLink(failedReductionID, -1);
//        updateDecisionNode(parentID);
//        for (int cand : parentStep.getUc())
//        {
//            Integer link = parentStep.getRemovalLink(cand);
//            if (link != null && link != -1 && link != -2)
//            {
//                addAndDistributeIrreducibilityInfo(link, failedReductionID);
//            }
//        }
//    }
    
    public synchronized void addAndDistributeReducibilityInfo(int childID, int reductionID, int link)
    {
        MUCStep childStep = retrieve(MUCStep.class, childID);
        childStep.setRemovalLink(reductionID, link);
        updateDecisionNode(childID);
        for (int edge : decisionGraph.getIncomingEdges(childID))
        {
            addAndDistributeReducibilityInfo(decisionGraph.getStartNode(edge), reductionID, -2);
        }
    }
    
    public synchronized void propagateIrreducibilityInfo(int parentID, int childID)
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
        updateDecisionNode(childID);
        for (int cand : childStep.getUc())
        {
            Integer link = childStep.getRemovalLink(cand);
            if (link != null && link != -1 && link != -2)
            {
                propagateIrreducibilityInfo(childID, link);
            }
        }
    }
    
    public synchronized void propagateReducibilityInfo(int childID, int parentID)
    {
        MUCStep childStep = retrieve(MUCStep.class, childID);
        MUCStep parentStep = retrieve(MUCStep.class, parentID);
        for (int cand : childStep.getUc())
        {
            Integer link = childStep.getRemovalLink(cand);
            if (link != null && link != -1 && link != 0)
            {
                if (parentStep.getRemovalLink(cand) == null)
                {
                    parentStep.setRemovalLink(cand, -2);
                }
            }
        }
        updateDecisionNode(parentID);
        for (int edge : decisionGraph.getIncomingEdges(parentID))
        {
            propagateReducibilityInfo(parentID, decisionGraph.getStartNode(edge));
        }
    }
    
    public void updateDecisionNode(int stepID)
    {
        MUCStep step = retrieve(MUCStep.class, stepID);
        int stepType = step.getStepType();
        String updatedCaption = step.getUc().size() + "";
        if (stepType == MUCStepType.UNKNOWN)
        {
            updatedCaption += " (" + step.numUnknownClauses() + ")";
        }
        decisionGraph.setNodeStatus(stepID, stepType);
        decisionGraph.setNodeCaption(stepID, updatedCaption);
    }
}
