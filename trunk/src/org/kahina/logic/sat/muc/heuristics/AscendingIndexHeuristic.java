package org.kahina.logic.sat.muc.heuristics;

import java.util.HashSet;
import java.util.Set;

public class AscendingIndexHeuristic extends ReductionHeuristic
{
    Set<Integer> alreadyProcessed;
    
    public AscendingIndexHeuristic()
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
    
    public void deliverCriticalClauses(Set<Integer> criticalClauses)
    {
        alreadyProcessed.addAll(criticalClauses);
    }
    
    public String getName()
    {
        return "ascending index heuristic";
    }
}
