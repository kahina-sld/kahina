package org.kahina.core.control;

public class KahinaActivationEvent extends KahinaEvent
{
    private final String elementID;
    private final boolean status;
    
    public KahinaActivationEvent(String elementID, boolean status)
    {
        super(KahinaEventTypes.ACTIVATION);
        this.elementID = elementID;
        this.status = status;
    }
    
    public String getElementID()
    {
        return elementID;
    }
    
    public boolean getStatus()
    {
        return status;
    }
    
    @Override
    public String toString()
    {
        return "activation: " + elementID + " to " + status;
    }
}
