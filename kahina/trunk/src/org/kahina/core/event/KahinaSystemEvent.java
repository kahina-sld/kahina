package org.kahina.core.event;

public class KahinaSystemEvent extends KahinaEvent
{
    int systemEventType;
    
    public static final int QUIT = 0;
    public static final int RESTART = 1;
    
    public KahinaSystemEvent(int systemEventType)
    {
        super("system");
        this.systemEventType = systemEventType;
    }
    
    public int getSystemEventType()
    {
        return systemEventType;
    }
    
    public String toString()
    {
        String s = "system: ";
        if (systemEventType == QUIT)
        {
            s += "quit"; 
        }
        else if (systemEventType == RESTART)
        {
            s += "restart parse"; 
        }
        else
        {
            s += "unknown operation";
        }
        return s;
    }
}
