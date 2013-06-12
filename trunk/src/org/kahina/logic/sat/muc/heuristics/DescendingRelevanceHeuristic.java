package org.kahina.logic.sat.muc.heuristics;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.kahina.logic.sat.data.proof.ResolutionProofTree;
import org.kahina.logic.sat.muc.MUCStep;

public class DescendingRelevanceHeuristic extends ReductionHeuristic
{
    Set<Integer> alreadyProcessed;
    boolean needsProof;
    List<Integer> relevanceList;
    int selVarOffset;
    
    public DescendingRelevanceHeuristic()
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
        System.err.println("New relevance list for DescendingRelevanceHeuristics: ");
        for (int var : relevanceList)
        {
            System.err.print(var + ",");
        }
        System.err.println();
        needsProof = false;
    }
    
    public void deliverCriticalClauses(Set<Integer> criticalClauses)
    {
        alreadyProcessed.addAll(criticalClauses);
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
            while (relevanceList.size() > 0)
            {
                int cand = relevanceList.remove(0) - selVarOffset;
                //System.err.print("cand "  + cand);
                //check if cand is an unused selection variable
                if (cand > 0 && !alreadyProcessed.contains(cand))
                {
                    //System.err.println(" - accepted");
                    alreadyProcessed.add(cand);
                    return cand;
                }
                //System.err.println(" - dismissed");
            }
        }
        return -1;
    }
    
    public String getName()
    {
        return "descending relevance heuristic";
    }
}
