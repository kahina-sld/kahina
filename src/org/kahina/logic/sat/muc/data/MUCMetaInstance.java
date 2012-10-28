package org.kahina.logic.sat.muc.data;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class MUCMetaInstance extends CnfSatInstance
{
    //TODO: consider recursive block hierarchies instead of partitions 
    //  + hierarchy of blocks allows for even shorter descriptions
    //  - expensive maintenance!
    
    //
    static final boolean VERBOSE = false;
    
    LiteralBlockHandler blockHandler;
    
    public MUCMetaInstance(int numOrigClauses)
    {
        super();
        setNumVars(numOrigClauses);
        this.blockHandler = new PartitionBlockHandler(this);
    }
    
    public void learnNewClause(List<Integer> clause)
    {
        clauses.add(blockHandler.buildRepresentation(clause));
    }
    
    public Collection<List<Integer>> getBlocks()
    {
        return blockHandler.getBlocks();
    }
    
}
