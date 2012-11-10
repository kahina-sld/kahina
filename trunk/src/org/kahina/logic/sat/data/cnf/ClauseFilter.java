package org.kahina.logic.sat.data.cnf;

public class ClauseFilter
{
    protected CnfSatInstance instance;
    
    public ClauseFilter(CnfSatInstance instance)
    {
        this.instance = instance;
    }
    
    public boolean acceptsClause(int clauseID)
    {
        return false;
    }
}
