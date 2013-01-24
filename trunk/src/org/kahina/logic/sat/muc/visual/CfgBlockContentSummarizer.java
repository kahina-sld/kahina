package org.kahina.logic.sat.muc.visual;

import java.util.Set;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class CfgBlockContentSummarizer extends BlockContentSummarizer
{
    public CfgBlockContentSummarizer(CnfSatInstance instance)
    {
        super(instance);
    }

    public String buildBlockSummary(Set<Integer> block)
    {
        StringBuilder s = new StringBuilder();
        s.append("[");
        if (block.size() < 3)
        {
            for (Integer literal : block)
            {
                s.append(instance.getSymbolForLiteral(literal));
                s.append(',');
            }
            s.deleteCharAt(s.length() - 1);
        }
        else
        {
            for (Integer literal : block)
            {
                s.append(literal);
                s.append(',');
            }
            s.deleteCharAt(s.length() - 1);
        }
        s.append(']');
        return s.toString();
    }
}
