package org.kahina.logic.sat.insertionmus.gui;


import org.kahina.core.control.KahinaEvent;

public class ClauseSelectionEvent extends KahinaEvent
{
    private int clauseID;
    
    public ClauseSelectionEvent(int clauseIDs)
    {
        super("clauseSelection");
        this.clauseID = clauseIDs;
    }

    public int getClauseID()
    {
        return clauseID;
    }
}
