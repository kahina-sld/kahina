package org.kahina.tulipa.event;

import org.kahina.core.event.KahinaEvent;

public class TulipaBridgeEvent  extends KahinaEvent
{
    //encode the event type with one of the constants from LogicProgrammingBridgeEventType
    private int eventType;
    
    //the tulipa step ID
    private int id;
    
    //fields for handing on different possible values
    private String strContent;
    private int intContent;
    private int intContent2;
    
    public TulipaBridgeEvent(int eventType)
    {
        super("tulipa bridge");
        this.eventType = eventType;
    }
    
    public TulipaBridgeEvent(int eventType, int externalID)
    {
        super("tulipa bridge");
        this.id = externalID;
        this.eventType = eventType;
    }
    
    public TulipaBridgeEvent(int eventType, int externalID, String strContent)
    {
        super("tulipa bridge");
        this.id = externalID;
        this.strContent = strContent;
        this.eventType = eventType;
    }
    
    public TulipaBridgeEvent(int eventType, int externalID, int intContent)
    {
        this(eventType,externalID);
        this.intContent = intContent;
    }
    
    public TulipaBridgeEvent(int eventType, int externalID, int intContent, int intContent2)
    {
        this(eventType,externalID);
        this.intContent = intContent;
        this.intContent2 = intContent2;
    }
    
    public int getEventType()
    {
        return eventType;
    }
    
    public int getID()
    {
        return id;
    }
    
    public String getStrContent()
    {
        return strContent;
    }
    
    public int getIntContent()
    {
        return intContent;
    }
    
    public int getIntContent2()
    {
        return intContent2;
    }
    
    public String toString()
    {
        String s = "tulipa bridge: ";
        switch (eventType)
        {
            case TulipaBridgeEventType.SET_ITEM_DESC:
            {
                s += "setItemDesc (" + id + ",\"" + strContent + "\")";
                break;
            }
            case TulipaBridgeEventType.SCAN_EPSILON:
            {
                s += "scanEpsilon (" + id + ",\"" + intContent + ")";
                break;
            }
            case TulipaBridgeEventType.SCAN:
            {
                s += "scan (" + id + ",\"" + intContent + ")";
                break;
            }
            case TulipaBridgeEventType.PREDICT:
            {
                s += "predict (" + id + ",\"" + intContent + ")";
                break;
            }
            case TulipaBridgeEventType.SUSPEND:
            {
                s += "suspend (" + id + ",\"" + intContent +  ",\"" + intContent2 + ")";
                break;
            }
            case TulipaBridgeEventType.RESUME:
            {
                s += "resume (" + id + ",\"" + intContent + ",\"" + intContent2 + ")";
                break;
            }
            case TulipaBridgeEventType.INIT:
            {
                s += "init";
                break;
            }
            case TulipaBridgeEventType.START:
            {
                s += "start (" + id + ")";
                break;
            }
        }
        return s;
    }
}
