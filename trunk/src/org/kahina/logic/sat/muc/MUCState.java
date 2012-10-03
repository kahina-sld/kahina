package org.kahina.logic.sat.muc;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kahina.core.KahinaState;
import org.kahina.core.control.KahinaController;
import org.kahina.core.data.dag.ColoredPathDAG;
import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.io.minisat.MiniSATFiles;
import org.kahina.logic.sat.muc.bridge.MUCInstruction;
import org.kahina.logic.sat.muc.data.MUCStatistics;
import org.kahina.logic.sat.muc.data.UCReducerList;

public class MUCState extends KahinaState
{
    public static boolean VERBOSE = false;
    
    CnfSatInstance satInstance;
    MUCStatistics stat;
    MiniSATFiles files;
    
    ColoredPathDAG decisionGraph;
    UCReducerList reducers;
    
    Map<MUCStep,Integer> nodeForStep;
    
    public MUCState(MUCInstance kahina)
    {
        super(kahina);
        this.satInstance = null;
        this.stat = null;
        this.files = null;
        this.nodeForStep = new HashMap<MUCStep,Integer>();
    }
    
    public MUCState(MUCInstance kahina, CnfSatInstance satInstance, MUCStatistics stat, MiniSATFiles files)
    {
        super(kahina);
        this.satInstance = satInstance;
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
        this.stat = null;
        this.files = null;
        this.nodeForStep = new HashMap<MUCStep,Integer>();
        initialize();
    }
    
    public CnfSatInstance getSatInstance()
    {
        return satInstance;
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
    
    public synchronized int registerMUC(MUCStep newStep, int parentID, int selCandidate)
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
                decisionGraph.addEdgeNoDuplicates(parentID, stepID, selCandidate + "");
                retrieve(MUCStep.class, parentID).setRemovalLink(selCandidate, stepID);
                propagateReducibilityInfo(parentID, stepID);
            }
        }
        else
        {
            if (VERBOSE) System.err.println("Adding decision graph edge (" + parentID + "," + stepID + ")");
            decisionGraph.addEdgeNoDuplicates(parentID, stepID, selCandidate + "");
            retrieve(MUCStep.class, parentID).setRemovalLink(selCandidate, stepID);
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
                propagateReducibilityInfo(parentID, stepID);
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
            propagateReducibilityInfo(parentID, stepID);
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
    }
    
    public void setStatistics(MUCStatistics stat)
    {
        this.stat = stat;
    }
    
    public void setFiles(MiniSATFiles files)
    {
        this.files = files;
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
