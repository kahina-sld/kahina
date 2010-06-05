package org.kahina.lp.event;

import org.kahina.core.event.KahinaEvent;

public class LogicProgrammingBridgeEvent extends KahinaEvent
{
    //encode the event type with one of the constants from LogicProgrammingBridgeEventType
    private int eventType;
    
    //the logic programming step ID
    private int id;
    
    //fields for handing on different possible values
    private String strContent;
    private int intContent;
    
    public LogicProgrammingBridgeEvent(int eventType, int externalID)
    {
        super("logic programming bridge");
        this.id = externalID;
        this.eventType = eventType;
    }
    
    public LogicProgrammingBridgeEvent(int eventType, int externalID, String strContent)
    {
        super("logic programming bridge");
        this.id = externalID;
        this.eventType = eventType;
        this.strContent = strContent;
    }
    
    public LogicProgrammingBridgeEvent(int eventType, int externalID, int intContent)
    {
        super("logic programming bridge");
        this.id = externalID;
        this.eventType = eventType;
        this.intContent = intContent;
    }
    
    public LogicProgrammingBridgeEvent(int eventType, int externalID, String strContent, int intContent)
    {
        super("logic programming bridge");
        this.id = externalID;
        this.eventType = eventType;
        this.strContent = strContent;
        this.intContent = intContent;
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
    
    public String toString()
    {
        String s = "lp bridge: ";
        switch (eventType)
        {
            case LogicProgrammingBridgeEventType.SET_GOAL_DESC:
            {
                s += "setGoalDesc (" + id + ",\"" + strContent + "\")";
                break;
            }
            case LogicProgrammingBridgeEventType.STEP_REDO:
            {
                s += "stepRedo (" + id + ")";
                break;
            }
            case LogicProgrammingBridgeEventType.STEP_DET_EXIT:
            {
                s += "stepDetExit (" + id + ")";
                break;
            }
            case LogicProgrammingBridgeEventType.STEP_NONDET_EXIT:
            {
                s += "stepNondetExit (" + id + ")";
                break;
            }
            case LogicProgrammingBridgeEventType.STEP_FAIL:
            {
                s += "stepFail (" + id + ")";
                break;
            }
        }
        return s;
    }
}
