package org.kahina.tulipa.behavior;

import org.kahina.core.behavior.KahinaDAGBehavior;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.data.dag.KahinaDAG;
import org.kahina.core.data.dag.KahinaMemDAG;
import org.kahina.tulipa.TulipaInstance;
import org.kahina.tulipa.TulipaStepStatus;
import org.kahina.tulipa.bridge.TulipaBridgeEvent;
import org.kahina.tulipa.bridge.TulipaBridgeEventType;

public class TulipaDAGBehavior extends KahinaDAGBehavior
{
    public static boolean VERBOSE = false;
    
    public TulipaDAGBehavior(KahinaDAG dag, TulipaInstance kahina)
    {
        super(dag, kahina);
        kahina.getControl().registerListener("dag", this);
        kahina.getControl().registerListener("tulipa bridge", this);
    }
    
    public void initializeDAG()
    {
        object.addNode(0,"start",0);
        //object.setRootID(0); FIXME method no longer exists 
    }
    
    public void processItemInformation(int itemID, String label)
    {
        object.addNode(itemID, label, TulipaStepStatus.PRODUCTIVE);
        // if (verbose) System.err.println(object.exportXML());
    }
    
    public void processStartItem(int itemID)
    {
        object.addEdge(0, itemID, "init");
        System.err.println(((KahinaMemDAG) object).exportXML());
    }
    
    public void processScanEpsilon(int itemID, int parentID)
    {
        object.addEdge(parentID, itemID, "scan_eps");
    }
    
    public void processScan(int itemID, int parentID)
    {
        object.addEdge(parentID, itemID, "scan");
    }
    
    public void processPredict(int itemID, int parentID)
    {
        object.addEdge(parentID, itemID, "predict");
    }
    
    public void processSuspend(int itemID, int parent1ID, int parent2ID)
    {
        object.addEdge(parent1ID, itemID, "suspend");
        object.addEdge(parent2ID, itemID, "suspend");
    }
    
    public void processResume(int itemID, int parent1ID, int parent2ID)
    {
        object.addEdge(parent1ID, itemID, "resume");
        object.addEdge(parent2ID, itemID, "resume");
    }
    
    public void processUnproductive(int itemID)
    {
        System.err.println("#####PROCESSING UNPRODUCTIVE");
        object.setNodeStatus(itemID, TulipaStepStatus.UNPRODUCTIVE);
        //TODO: make sure "unproductive" items are not accessed via suspend or resume
        //object.collapse(itemID);
        //also set ancestors with only unproductive descendants to "unproductive"
        for (int edgeID : object.getIncomingEdges(itemID))
        {
            int parentID = object.getStartNode(edgeID);
            if (allChildrenAreUnproductive(parentID))
            {
                processUnproductive(parentID);
            }
        }
    }
    
    private boolean allChildrenAreUnproductive(int itemID)
    {
        boolean unproductive = true;
        for (int edgeID : object.getOutgoingEdges(itemID))
        {
            int childID = object.getEndNode(edgeID);
            if (object.getNodeStatus(childID) == TulipaStepStatus.PRODUCTIVE)
            {
                unproductive = false;
            }
        }
        return unproductive;
    }
    
    @Override
	public void processEvent(KahinaEvent e)
    {
        if (VERBOSE)
            System.err.println("TulipaDAGBehavior.processEvent(" + e + ")");
        if (e instanceof TulipaBridgeEvent)
        {
            processEvent((TulipaBridgeEvent) e);
        }
    }

    public void processEvent(TulipaBridgeEvent e)
    {
        switch (e.getEventType())
        {
            case TulipaBridgeEventType.SET_ITEM_DESC:
            {
                processItemInformation(e.getID(), e.getStrContent());
                break;
            }
            case TulipaBridgeEventType.SCAN_EPSILON:
            {
                processScanEpsilon(e.getID(), e.getIntContent());
                break;
            }
            case TulipaBridgeEventType.SCAN:
            {
                processScan(e.getID(), e.getIntContent());
                break;
            }
            case TulipaBridgeEventType.PREDICT:
            {
                processPredict(e.getID(), e.getIntContent());
                break;
            }
            case TulipaBridgeEventType.SUSPEND:
            {
                processSuspend(e.getID(), e.getIntContent(), e.getIntContent2());
                break;
            }
            case TulipaBridgeEventType.RESUME:
            {
                processResume(e.getID(), e.getIntContent(), e.getIntContent2());
                break;
            }
            case TulipaBridgeEventType.INIT:
            {
                initializeDAG();
                break;
            }
            case TulipaBridgeEventType.START:
            {
                processStartItem(e.getID());
                break;
            }
            case TulipaBridgeEventType.UNPRODUCTIVE:
            {
                processUnproductive(e.getID());
                break;
            }
        }
    }
}
