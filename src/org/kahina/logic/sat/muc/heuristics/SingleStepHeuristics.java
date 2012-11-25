package org.kahina.logic.sat.muc.heuristics;

public class SingleStepHeuristics extends ReductionHeuristics
{
    int toReduce;
    boolean done;
    
    public SingleStepHeuristics(int toReduce)
    {
        this.toReduce = toReduce;
        this.done = false;
    }
    
    @Override
    public int getNextCandidate()
    {
        if (!done)
        {
            done = true;
            return toReduce;
        }
        else return -1;
    }
}
