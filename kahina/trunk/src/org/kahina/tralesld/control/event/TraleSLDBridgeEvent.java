package org.kahina.tralesld.control.event;

import org.kahina.core.event.KahinaEvent;

public class TraleSLDBridgeEvent extends KahinaEvent
{
    // encode the event type with one of the constants from TraleSLDBridgeEventType
    private int eventType;
    
    //the logic programming step ID
    private int internalID;
    
    //fields for handing on different possible values
    private String strContent;
    private int intContent;
    
    public TraleSLDBridgeEvent(int eventType, int internalID)
    {
    	super("traleSLD bridge");
    	this.eventType = eventType;
    	this.internalID = internalID;
    }
    
    public TraleSLDBridgeEvent(int eventType, int internalID, String strContent)
    {
        super("traleSLD bridge");
        this.eventType = eventType;
        this.internalID = internalID;
        this.strContent = strContent;
    }
    
    public TraleSLDBridgeEvent(int eventType, int internalID, String strContent, int intContent)
    {
        super("traleSLD bridge");
        this.eventType = eventType;
        this.internalID = internalID;
        this.strContent = strContent;
        this.intContent = intContent;
    }
    
    public int getEventType()
    {
        return eventType;
    }
    
    public int getInternalID()
    {
        return internalID;
    }
    
    public String getStrContent()
    {
        return strContent;
    }
    
    public int getIntContent()
    {
        return intContent;
    }
    
    @Override
	public String toString()
    {
        String s = "tralesld bridge: ";
        switch (eventType)
        {
            case TraleSLDBridgeEventType.RULE_APP:
            {
                s += "ruleApplication(" + internalID + ",\"" + strContent + "\")";
                break;
            }
            case TraleSLDBridgeEventType.INIT:
            {
                s += "init(" + strContent + ")";
                break;
            }
            case TraleSLDBridgeEventType.STEP_FINISHED:
            {
                s += "stepFinished (" + internalID + ")";
                break;
            }
        }
        return s;
    }
}
