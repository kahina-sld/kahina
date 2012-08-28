package org.kahina.core.control;

public class KahinaStepUpdateEvent extends KahinaEvent
{
    private final int stepID;
    
    public KahinaStepUpdateEvent(int stepID)
    {
        super(KahinaEventTypes.STEP_UPDATE);
        this.stepID = stepID;
    }
    
    public int getStepID()
    {
        return stepID;
    }
    
    @Override
    public String toString()
    {
        return "step update: " + stepID;
    }
}
