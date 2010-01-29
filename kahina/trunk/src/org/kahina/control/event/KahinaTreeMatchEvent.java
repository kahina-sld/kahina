package org.kahina.control.event;

import org.kahina.breakpoint.TreeAutomaton;

public class KahinaTreeMatchEvent extends KahinaEvent
{
    TreeAutomaton aut;
    int nodeID;
    
    public KahinaTreeMatchEvent(TreeAutomaton aut, int nodeID)
    {
        super("treeMatch");
        this.aut = aut;
        this.nodeID = nodeID;
    }
}
