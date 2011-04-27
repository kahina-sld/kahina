package org.kahina.core.event;

public class KahinaWindowEvent extends KahinaEvent
{
	int windowEventType;
	String windowID;
	
    public KahinaWindowEvent(int type, String windowID)
    {
        super(KahinaEventTypes.WINDOW);
        this.windowEventType = type;
        this.windowID = windowID;
    }
	
	public int getWindowEventType()
	{
		return windowEventType;
	}
	
	public String getWindowID()
	{
		return windowID;
	}
}
