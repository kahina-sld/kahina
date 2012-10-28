package org.kahina.logic.sat.muc.data;

import java.util.Collection;
import java.util.List;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public abstract class LiteralBlockHandler
{
    protected CnfSatInstance satInstance;

    public LiteralBlockHandler(CnfSatInstance satInstance)
    {
        this.satInstance = satInstance;
    }
    
    public abstract List<Integer> buildRepresentation(List<Integer> clause);
    
    public abstract Collection<List<Integer>> getBlocks();
}
