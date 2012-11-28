package org.kahina.logic.sat.muc.heuristics;

public class SingleStepHeuristic extends ReductionHeuristic
{
    int toReduce;
    boolean done;
    
    public SingleStepHeuristic(int toReduce)
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
