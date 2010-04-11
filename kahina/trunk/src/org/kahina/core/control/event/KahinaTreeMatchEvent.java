package org.kahina.core.control.event;

import org.kahina.core.breakpoint.KahinaBreakpoint;

public class KahinaTreeMatchEvent extends KahinaEvent
{
    KahinaBreakpoint bp;
    int nodeID;
    
    public KahinaTreeMatchEvent(KahinaBreakpoint bp, int nodeID)
    {
        super("treeMatch");
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
}
