package org.kahina.core.control;


public class KahinaSystemEvent extends KahinaEvent
{
    int systemEventType;
    int intContent;
    
    public static final int QUIT = 0;
    public static final int APPLY_BREAKPOINTS = 1;
    
    public KahinaSystemEvent(int systemEventType)
    {
        super("system");
        this.systemEventType = systemEventType;
        this.intContent = -1;
    }
    
    public KahinaSystemEvent(int systemEventType, int intContent)
    {
        super("system");
        this.systemEventType = systemEventType;
        this.intContent = intContent;
    }
    
    public int getSystemEventType()
    {
        return systemEventType;
    }
    
    public int getIntContent()
    {
        return intContent;
    }
    
    @Override
	public String toString()
    {
        String s = "system: ";
        if (systemEventType == QUIT)
        {
            s += "quit"; 
        }
        else if (systemEventType == APPLY_BREAKPOINTS)
        {
            s += "apply breakpoints of type " + intContent; 
        }
        else
        {
            s += "unknown operation";
        }
        return s;
    }
}
