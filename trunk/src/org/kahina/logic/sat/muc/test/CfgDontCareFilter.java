package org.kahina.logic.sat.muc.test;

import java.util.List;

import org.kahina.logic.sat.data.cnf.ClauseFilter;
import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class CfgDontCareFilter extends ClauseFilter
{
    public CfgDontCareFilter(CnfSatInstance instance)
    {
        super(instance);
    }

    public boolean acceptsClause(int clauseID)
    {
        List<Integer> clause = instance.getClause(clauseID);
        for (int literal : clause)
        {
            String symbol = instance.getSymbolForLiteral(literal);
            if (!symbol.contains("]:")) return false;
        }
        return true;
    }
}
