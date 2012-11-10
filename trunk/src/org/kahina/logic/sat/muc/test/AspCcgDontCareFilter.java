package org.kahina.logic.sat.muc.test;

import java.util.List;

import org.kahina.logic.sat.data.cnf.ClauseFilter;
import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class AspCcgDontCareFilter extends ClauseFilter
{
    public AspCcgDontCareFilter(CnfSatInstance instance)
    {
        super(instance);
    }

    public boolean acceptsClause(int clauseID)
    {
        List<Integer> clause = instance.getClauses().get(clauseID);
        for (int literal : clause)
        {
            String symbol = instance.getSymbolForLiteral(literal);
            if (symbol.contains("position_category")) return false;
            if (symbol.contains("occurs")) return false;
        }
        return true;
    }
}
