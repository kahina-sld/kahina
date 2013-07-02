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
    
    /**
     * 
     * @return
     */
    public boolean needsProof()
    {
        return false;
    }
    
    /**
     * Determines whether the heuristic wants to be informed about 
     * @return
     */
    public boolean usesProofs()
    {
        return false;
    }
    
    /**
     * Is called by the reduction agent to hand on proof trees to the heuristic
     * for complex calculations, if usesProofs() is defined to return true.
     * @param proof
     */
    public void deliverProof(ResolutionProofTree proof)
    {
        
    }
    
    /**
     * Does nothing by default
     * @param criticalClauses
     */
    public void deliverCriticalClauses(Set<Integer> criticalClauses)
    {
        
    }
    
    /**
     * Is used by the reduction agent steered by this heuristic to poll
     * reduction candidates. This is the main interface for defining a heuristic,
     * and it may depend on arbitrarily complex internal calculations.
     * This method is abstract, so that every heuristic needs to implement it.
     * @return the next reduction candidate to be tried, -1 to terminate.
     */
    public abstract int getNextCandidate();
    
    /**
     * Defines the name of the heuristic as displayed in the reduction agent system.
     * @return the name of the heuristic.
     */
    public String getName()
    {
        return "unkown heuristic";
    }
}
