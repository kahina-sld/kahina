package org.kahina.core.control.event;

public class LogicProgrammingBridgeEvent extends KahinaEvent
{
    //encode the event type with one of the constants from LogicProgrammingBridgeEventType
    private int eventType;
    
    //the logic programming step ID
    private int externalID;
    
    //fields for handing on different possible values
    private String strContent;
    private int intContent;
    
    public LogicProgrammingBridgeEvent(int eventType, int externalID)
    {
        super("logic programming bridge");
        this.eventType = eventType;
    }
    
    public LogicProgrammingBridgeEvent(int eventType, int externalID, String strContent)
    {
        super("logic programming bridge");
        this.eventType = eventType;
        this.strContent = strContent;
    }
    
    public LogicProgrammingBridgeEvent(int eventType, int externalID, int intContent)
    {
        super("logic programming bridge");
        this.eventType = eventType;
        this.intContent = intContent;
    }
    
    public LogicProgrammingBridgeEvent(int eventType, int externalID, String strContent, int intContent)
    {
        super("logic programming bridge");
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
