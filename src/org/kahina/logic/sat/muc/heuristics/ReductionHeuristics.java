package org.kahina.logic.sat.muc.heuristics;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.muc.MUCStep;

public abstract class ReductionHeuristics
{
    CnfSatInstance satInstance;
    MUCStep uc;
    
    public void setNewUC(MUCStep uc)
    {
        this.uc = uc;
    }
    
    public abstract int getNextCandidate();
    
    public String getName()
    {
        return "unkown heuristic";
    }
}
