package org.kahina.logic.sat.muc.heuristics;

import java.util.HashSet;
import java.util.Set;

public class AlwaysFirstHeuristics extends UCReductionHeuristics
{
    Set<Integer> alreadyProcessed;
    
    public AlwaysFirstHeuristics()
    {
        alreadyProcessed = new HashSet<Integer>();
    }
    
    @Override
    public int getNextCandidate()
    {
        for (int ic : uc.getUc())
        {
            if (!alreadyProcessed.contains(ic))
            {
                alreadyProcessed.add(ic);
                return ic;
            }
        }
        return -1;
    }
    
    public String getName()
    {
        return "always-first heuristic";
    }
}
