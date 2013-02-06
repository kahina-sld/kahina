package org.kahina.logic.sat.muc.heuristics;

import java.util.List;
import java.util.Set;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.data.proof.ResolutionProofTree;
import org.kahina.logic.sat.muc.MUCStep;

public abstract class ReductionHeuristic
{
    CnfSatInstance satInstance;
    MUCStep uc;
    
    public void setSelVarOffset(int selVarOffset)
    {
        
    }
    
    public void setNewUC(MUCStep uc)
    {
        this.uc = uc;
    }
    
    public boolean needsProof()
    {
        return false;
    }
    
    public boolean usesProofs()
    {
        return false;
    }
    
    public void deliverProof(ResolutionProofTree proof)
    {
        
    }
    
    public void deliverCriticalClauses(Set<Integer> criticalClauses)
    {
        
    }
    
    //-1 is the signal that we are ready
    public abstract int getNextCandidate();
    
    public String getName()
    {
        return "unkown heuristic";
    }
}
