package org.kahina.core.event;

import org.kahina.core.control.KahinaBreakpoint;

public class KahinaTreeMatchEvent extends KahinaEvent
{
    KahinaBreakpoint bp;
    int nodeID;
    
    public KahinaTreeMatchEvent(KahinaBreakpoint bp, int nodeID)
    {
        super(KahinaEventTypes.TREE_MATCH);
        this.bp = bp;
        this.nodeID = nodeID;
    }

    public KahinaBreakpoint getBreakpoint()
    {
        return bp;
    }

    public int getNodeID()
    {
        return nodeID;
    }
    
    @Override
    public String toString()
    {
    	return bp + " matched at node " + nodeID;
    }
}
