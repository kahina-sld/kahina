package org.kahina.logic.sat.muc.visual;

import java.util.Set;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class BlockContentSummarizer
{
    CnfSatInstance instance;
    
    public BlockContentSummarizer(CnfSatInstance instance)
    {
        this.instance = instance;
    }
    
    public String buildBlockSummary(Set<Integer> block)
    {
        StringBuilder s = new StringBuilder();
        s.append("[");
        for (Integer literal : block)
        {
            s.append(literal);
            s.append(',');
        }
        s.deleteCharAt(s.length() - 1);
        s.append(']');
        return s.toString();
    }
}
