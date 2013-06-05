package org.kahina.logic.sat.insertionmus.gui;

import java.util.List;

import org.kahina.core.control.KahinaEvent;

public class ClauseSelectionEvent extends KahinaEvent
{
    private List<Integer> clauseIDs;
    
    public ClauseSelectionEvent(List<Integer> clauseIDs)
    {
        super("clauseSelection");
        this.clauseIDs = clauseIDs;
    }

    public List<Integer> getClauseIDs()
    {
        return clauseIDs;
    }
}
