package org.kahina.lp.event;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

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
    private Map<Integer, Integer> stepIDConv;
    
    public LogicProgrammingBridgeEvent(int eventType, int internalID)
    {
        super("logic programming bridge");
        this.id = internalID;
        this.eventType = eventType;
    }
    
    public LogicProgrammingBridgeEvent(int eventType, int internalID, String strContent)
    {
        super("logic programming bridge");
        this.id = internalID;
        this.eventType = eventType;
        this.strContent = strContent;
    }
    
    public LogicProgrammingBridgeEvent(int eventType, int internalID, int intContent)
    {
        super("logic programming bridge");
        this.id = internalID;
        this.eventType = eventType;
        this.intContent = intContent;
    }
    
    public LogicProgrammingBridgeEvent(int eventType, int internalID, String strContent, int intContent)
    {
        super("logic programming bridge");
        this.id = internalID;
        this.eventType = eventType;
        this.strContent = strContent;
        this.intContent = intContent;
    }
    
    public LogicProgrammingBridgeEvent(int eventType, int internalID, HashMap<Integer, Integer> stepIDConv)
	{
        this(eventType, internalID);
        this.stepIDConv = stepIDConv;
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
    
    public int getInternalIDForExternalID(int externalID)
    {
    	return stepIDConv.get(externalID);
    }

	public Map<Integer, Integer> getStepIDConv()
	{
		return Collections.unmodifiableMap(stepIDConv);
	}
}
