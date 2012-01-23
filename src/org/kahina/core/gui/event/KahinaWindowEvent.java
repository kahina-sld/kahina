package org.kahina.core.gui.event;

import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;

public class KahinaWindowEvent extends KahinaEvent
{
	int windowEventType;
	int windowID;
	
	String stringContent;
	
    public KahinaWindowEvent(int type, int windowID)
    {
        super(KahinaEventTypes.WINDOW);
        this.windowEventType = type;
        this.windowID = windowID;
    }
    
    public KahinaWindowEvent(int type, int windowID, String stringContent)
    {
        super(KahinaEventTypes.WINDOW);
        this.windowEventType = type;
        this.windowID = windowID;
        this.stringContent = stringContent;
    }
	
	public int getWindowEventType()
	{
		return windowEventType;
	}
	
	public int getWindowID()
	{
		return windowID;
	}
	
	public String getStringContent()
	{
		return stringContent;
	}
}
