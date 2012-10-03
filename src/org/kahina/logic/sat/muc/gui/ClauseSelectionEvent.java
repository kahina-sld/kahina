package org.kahina.logic.sat.muc.gui;

import org.kahina.core.control.KahinaEvent;

public class ClauseSelectionEvent extends KahinaEvent
{
    private int clauseID;
    
    public ClauseSelectionEvent(int clauseID)
    {
        super("clauseSelection");
        this.clauseID = clauseID;
    }

    public int getClauseID()
    {
        return clauseID;
    }
}
