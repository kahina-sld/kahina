package org.kahina.logic.sat.muc.heuristics;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class CenteredIndexHeuristic extends ReductionHeuristic
{
    Set<Integer> alreadyProcessed;
    
    public CenteredIndexHeuristic()
    {
        alreadyProcessed = new HashSet<Integer>();
    }
    
    @Override
    public int getNextCandidate()
    {
        List<Integer> list = uc.getUc();
        int idx =  list.size() / 2;
        int cand = list.get(idx);
        if (!alreadyProcessed.contains(cand))
        {
            alreadyProcessed.add(cand);
            return cand;
        }
        for (int i = 1; i < idx; i++)
        {
            cand = list.get(idx + i);
            if (!alreadyProcessed.contains(cand))
            {
                alreadyProcessed.add(cand);
                return cand;
            }
            cand = list.get(idx - i);
            if (!alreadyProcessed.contains(cand))
            {
                alreadyProcessed.add(cand);
                return cand;
            }
        }
        cand = list.get(0);
        if (!alreadyProcessed.contains(cand))
        {
            alreadyProcessed.add(cand);
            return cand;
        }
        cand = list.get(list.size() - 1);
        if (!alreadyProcessed.contains(cand))
        {
            alreadyProcessed.add(cand);
            return cand;
        }
        return -1;
    }
    
    public void deliverCriticalClauses(Set<Integer> criticalClauses)
    {
        alreadyProcessed.addAll(criticalClauses);
    }
    
    public String getName()
    {
        return "centered index heuristic";
    }
}
