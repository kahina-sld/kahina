package org.kahina.tulipa.behavior;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaRunner;
import org.kahina.core.behavior.KahinaDAGBehavior;
import org.kahina.core.data.dag.KahinaDAG;
import org.kahina.core.data.dag.KahinaMemDAG;
import org.kahina.core.event.KahinaEvent;
import org.kahina.tulipa.event.TulipaBridgeEvent;
import org.kahina.tulipa.event.TulipaBridgeEventType;

public class TulipaDAGBehavior extends KahinaDAGBehavior
{
    public static boolean verbose = true;
    
    public TulipaDAGBehavior(KahinaDAG dag, KahinaInstance kahina)
    {
        super(dag, kahina);
        KahinaRunner.getControl().registerListener("tulipa bridge", this);
    }
    
    public void initializeDAG()
    {
        object.addNode(0,"start",0);
        object.setRootID(0);
    }
    
    public void processItemInformation(int itemID, String label)
    {
        //TODO: do something useful with item status
        object.addNode(itemID, label, 0);
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
    
    public void processEvent(KahinaEvent e)
    {
        if (verbose)
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
        }
    }
}
