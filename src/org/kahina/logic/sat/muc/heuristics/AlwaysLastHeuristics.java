package org.kahina.logic.sat.muc.heuristics;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class AlwaysLastHeuristics extends ReductionHeuristics
{
    Set<Integer> alreadyProcessed;
    
    public AlwaysLastHeuristics()
    {
        alreadyProcessed = new HashSet<Integer>();
    }

    @Override
    public int getNextCandidate()
    {
        List<Integer> list = uc.getUc();
        for (int i = list.size() - 1; i >= 0; i--)
        {
            int ic = list.get(i);
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
        return "always-last heuristic";
    }
}
