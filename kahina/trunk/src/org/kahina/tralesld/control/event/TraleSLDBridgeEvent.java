package org.kahina.tralesld.control.event;

import org.kahina.core.control.event.KahinaEvent;

public class TraleSLDBridgeEvent extends KahinaEvent
{
    // encode the event type with one of the constants from TraleSLDBridgeEventType
    private int eventType;
    
    //the logic programming step ID
    private int externalID;
    
    //fields for handing on different possible values
    private String strContent;
    private int intContent;
    
    public TraleSLDBridgeEvent(int eventType, int externalID, String strContent)
    {
        super("traleSLD bridge");
        this.eventType = eventType;
        this.strContent = strContent;
    }
    
    public TraleSLDBridgeEvent(int eventType, int externalID, String strContent, int intContent)
    {
        super("traleSLD bridge");
        this.eventType = eventType;
        this.strContent = strContent;
        this.intContent = intContent;
    }
    
    public int getEventType()
    {
        return eventType;
    }
    
    public int getExternalID()
    {
        return externalID;
    }
    
    public String getStrContent()
    {
        return strContent;
    }
    
    public int getIntContent()
    {
        return intContent;
    }
}
