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
        for (int i = 0; i < clause.size(); i++)
        {
            //distribution symbol in the consequent -> filter out (just a bridge var)
            if (i > 1)
            {
                String symbol = instance.getSymbolForLiteral(clause.get(i));
                if (symbol.contains(":")) return true;
            }
        }
        //select clauses of size two where both literals are negative
        //(this makes the exclusivity of symbols implicit)
        if (clause.size() == 2)
        {
            return (clause.get(0) < 0 && clause.get(1) < 0);
        }
        return false;
    }
}
