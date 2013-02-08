package org.kahina.logic.sat.muc.bridge;

import java.util.LinkedList;

import org.kahina.core.bridge.KahinaBridge;
import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.muc.MUCState;
import org.kahina.logic.sat.muc.MUCStep;
import org.kahina.logic.sat.muc.gui.ClauseSelectionEvent;

public class MUCBridge extends KahinaBridge
{
    private MUCState state;
    
    // always contains the internal ID of the selected step
    public int selectedID = -1;
    //store the last instruction, so we know where the arriving step data belong
    protected MUCInstruction lastInstruction;
    //store the selected candidate; 
    //this is filled via a CONTROL event by the GUI, 
    //and forms the most important part of the MUCInstructions handed on to the MinUnsatCore algorithm
    protected int selectedCandidate = -1;
    
    //a special mode in which data are processed without firing selection events
    protected boolean batchMode = false;
    LinkedList<Integer> batchQueue = new LinkedList<Integer>();

    public MUCBridge(MUCInstance kahina)
    {
        super(kahina);
        this.state = (MUCState) kahina.getState();
        kahina.registerSessionListener(KahinaEventTypes.SYSTEM, this);
        kahina.registerInstanceListener(KahinaEventTypes.SELECTION, this);
    }
    
    //receive muc_cand and muc lists for a step where UC reduction was successful
    public int registerMUC(Integer[] mucCandidates, Integer[] muc)
    {
        //System.err.println("registerMUC(" + mucCandidates.length + "," + muc.length + ")");
        int stepID = state.registerMUC(mucCandidates, muc, lastInstruction, selectedID);
        if (batchQueue.size() == 0)
        {
            //automatically select the new step (allows marching down to a MUS using only the control panel)
            kahina.dispatchEvent(new KahinaUpdateEvent(selectedID));
            kahina.dispatchEvent(new KahinaSelectionEvent(stepID));
            kahina.dispatchEvent(new ClauseSelectionEvent(new LinkedList<Integer>()));
            kahina.dispatchEvent(new KahinaUpdateEvent(stepID));
        }
        return stepID;
    }
    
    //receive information that IC removal led to satisfiability
    public void registerSatisfiable()
    {
        //MUCStep newStep = new MUCStep();
        //int stepID = state.nextStepID();
        //state.store(stepID, newStep);
        //state.getDecisionGraph().addNode(stepID, "SAT", MUCStepType.SAT);
        //state.getDecisionGraph().addEdge(selectedID, stepID, lastInstruction.selCandidate + "");
        MUCStep parentStep = state.retrieve(MUCStep.class, selectedID);
        parentStep.setRemovalLink(lastInstruction.selCandidate, -1);
        if (batchQueue.size() == 0)
        {
            //do NOT automatically select the new step (there would be no options for expansion)
            kahina.dispatchEvent(new KahinaUpdateEvent(selectedID));
            kahina.dispatchEvent(new KahinaSelectionEvent(selectedID));
            kahina.dispatchEvent(new ClauseSelectionEvent(new LinkedList<Integer>()));
            kahina.dispatchEvent(new KahinaUpdateEvent(selectedID));
        }
    }
    
    //purge the decision tree of all SAT nodes
    public void purgeSATNodes()
    {
        System.err.println("Removal of leaves not yet implemented for DAGs!");
        /*KahinaDAG graph = state.getDecisionGraph();
        for (int leaf : graph.getLeaves())
        {
            if (graph.getNodeCaption(leaf).equals("SAT"))
            {
                if (leaf == selectedID)
                {
                    selectedID = graph.getParent(leaf,0);
                }
                boolean success = graph.removeLeaf(leaf);
                if (!success)
                {
                    System.err.println("Failed to remove a SAT leaf!");
                }
            }
        }
        kahina.dispatchEvent(new KahinaUpdateEvent(selectedID));
        kahina.dispatchEvent(new KahinaRedrawEvent());*/
    }
    
    /**
     * Returns a step instruction determined by the user of the MUCGUI.
     * 
     * Is used by the MinUnsatCore algorithm to prompt the bridge for the next step.
     * @return null, if no new instruction, the next MUCInstruction otherwise
     */
    public MUCInstruction getNextInstruction()
    {
        //System.err.println("MUCBridge.getNextInstruction()");
        if (batchQueue.size() > 0)
        {
            lastInstruction = new MUCInstruction();
            //TODO: this can be made a lot more efficient because selectedID does not change in batch mode
            lastInstruction.step = state.retrieve(MUCStep.class, selectedID);
            lastInstruction.selCandidate = batchQueue.remove(0);
            return lastInstruction;
        }
        else if (selectedCandidate != -1)
        {
           System.err.println("selected candidate: " + selectedCandidate);
           lastInstruction = new MUCInstruction();
           lastInstruction.step = state.retrieve(MUCStep.class, selectedID);
           lastInstruction.selCandidate = selectedCandidate;
           selectedCandidate = -1;
           return lastInstruction;
        }
        return null;
    }
    
    public synchronized void processEvent(KahinaEvent e)
    {
        if (e instanceof KahinaSelectionEvent)
        {
            processSelectionEvent((KahinaSelectionEvent) e);
        }
        else if (e instanceof KahinaControlEvent)
        {
            processControlEvent((KahinaControlEvent) e);
        }
    }
    
    //SELECTION event changes the selectedID
    protected synchronized void processSelectionEvent(KahinaSelectionEvent e)
    {
        //selection during batch mode would cause nodes to erroneously arrive at the new selection
        if (batchQueue.size() == 0)
        {
            //TODO: find out why the selection event does not reach the state!
            //state.processEvent(e);
            selectedID = e.getSelectedStep();
        }
    }
    
    //CONTROL event changes the selectedCandidate
    protected synchronized void processControlEvent(KahinaControlEvent e)
    {
        String command = e.getCommand();
        if (command.equals("begin"))
        {
            batchQueue.clear();
            batchMode = true;
        }
        else if (command.equals("end"))
        {
            batchMode = false;
            kahina.dispatchEvent(new KahinaUpdateEvent(selectedID));
            kahina.dispatchEvent(new KahinaSelectionEvent(selectedID));
        }
        else if (command.equals("purge"))
        {
            purgeSATNodes();
        }
        else
        {
            try
            {
                if (batchMode)
                {
                    batchQueue.add(Integer.parseInt(command));
                }
                else
                {
                    selectedCandidate = Integer.parseInt(command);
                }
            }
            catch (NumberFormatException exc)
            {
                //do nothing: such a control event is not processed by the bridge
            }
        }
    }
    
    public synchronized boolean isInBatchMode()
    {
        System.err.println("batch queue: " + batchQueue);
        return batchQueue.size() != 0;
    }
}
