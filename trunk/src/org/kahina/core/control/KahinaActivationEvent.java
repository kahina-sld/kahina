package org.kahina.core.control;

public class KahinaActivationEvent extends KahinaEvent
{
    private final String elementID;
    private final KahinaActivationStatus status;
    
    public KahinaActivationEvent(String elementID, KahinaActivationStatus status)
    {
        super(KahinaEventTypes.ACTIVATION);
        this.elementID = elementID;
        this.status = status;
    }
    
    public String getElementID()
    {
        return elementID;
    }
    
    public KahinaActivationStatus getStatus()
    {
        return status;
    }
    
    @Override
    public String toString()
    {
        return "activation: " + elementID + " to " + status;
    }
}
