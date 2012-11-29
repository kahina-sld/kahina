package org.kahina.logic.sat.muc.heuristics;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.kahina.logic.sat.data.proof.ResolutionProofTree;
import org.kahina.logic.sat.muc.MUCStep;

public class CenteredRelevanceHeuristic extends ReductionHeuristic
{
    Set<Integer> alreadyProcessed;
    boolean needsProof;
    List<Integer> relevanceList;
    int selVarOffset;
    
    public CenteredRelevanceHeuristic()
    {
        alreadyProcessed = new HashSet<Integer>();
        needsProof = true;
        relevanceList = null;
    }
    
    public void setSelVarOffset(int selVarOffset)
    {
        this.selVarOffset = selVarOffset;
    }
    
    public void setNewUC(MUCStep uc)
    {
        if (this.uc != uc)
        {
            this.uc = uc;
            needsProof = true;
        }
    }
    
    public boolean needsProof()
    {
        return needsProof;
    }
    
    public boolean usesProofs()
    {
        return true;
    }
    
    public void deliverProof(ResolutionProofTree proof)
    {
        relevanceList = proof.getVarRelevanceOrdering();
        /*System.err.println("New relevance list for DescendingRelevanceHeuristics: ");
        for (int var : relevanceList)
        {
            System.err.print(var + ",");
        }
        System.err.println();*/
        needsProof = false;
    }
    
    @Override
    public int getNextCandidate()
    {
        if (relevanceList == null)
        {
            for (int ic : uc.getUc())
            {
                if (!alreadyProcessed.contains(ic))
                {
                    alreadyProcessed.add(ic);
                    return ic;
                }
            }
        }
        else
        {
            int idx =  relevanceList.size() / 2;
            int cand = relevanceList.get(idx) - selVarOffset;
            if (cand > 0 && !alreadyProcessed.contains(cand))
            {
                alreadyProcessed.add(cand);
                return cand;
            }
            for (int i = 1; i < idx; i++)
            {
                cand = relevanceList.get(idx + i) - selVarOffset;
                if (cand > 0 && !alreadyProcessed.contains(cand))
                {
                    alreadyProcessed.add(cand);
                    return cand;
                }
                cand = relevanceList.get(idx - i) - selVarOffset;
                if (cand > 0 && !alreadyProcessed.contains(cand))
                {
                    alreadyProcessed.add(cand);
                    return cand;
                }
            }
            cand = relevanceList.get(0) - selVarOffset;
            if (cand > 0 && !alreadyProcessed.contains(cand))
            {
                alreadyProcessed.add(cand);
                return cand;
            }
            cand = relevanceList.get(relevanceList.size() - 1) - selVarOffset;
            if (!alreadyProcessed.contains(cand))
            {
                alreadyProcessed.add(cand);
                return cand;
            }
        }
        return -1;
    }
    
    public String getName()
    {
        return "centered relevance heuristic";
    }
}
