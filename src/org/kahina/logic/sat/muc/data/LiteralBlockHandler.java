package org.kahina.logic.sat.muc.data;

import java.util.Collection;
import java.util.List;
import java.util.TreeSet;

import org.kahina.core.data.KahinaObject;
import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public abstract class LiteralBlockHandler extends KahinaObject
{
    protected CnfSatInstance satInstance;

    public LiteralBlockHandler(CnfSatInstance satInstance)
    {
        this.satInstance = satInstance;
    }
    
    public abstract List<Integer> buildRepresentation(TreeSet<Integer> clause);
    
    public abstract Collection<TreeSet<Integer>> getBlocks();
    
    public abstract TreeSet<Integer> getBlock(int blockID);
}
