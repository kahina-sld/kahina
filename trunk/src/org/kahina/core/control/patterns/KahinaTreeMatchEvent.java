package org.kahina.core.control.patterns;

import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.data.breakpoint.KahinaBreakpoint;

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
