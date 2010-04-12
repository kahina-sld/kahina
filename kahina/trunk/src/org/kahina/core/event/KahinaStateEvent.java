package org.kahina.core.event;

import java.io.File;

public class KahinaStateEvent extends KahinaEvent
{
    File file;
    int stateEventType;
    
    public static final int SAVE_STATE = 0;
    public static final int LOAD_STATE = 1;
    
    public KahinaStateEvent(int stateEventType, File file)
    {
        super("state");
        this.stateEventType = stateEventType;
        this.file = file;
    }
    
    public File getFile()
    {
        return file;
    }
    
    public int getStateEventType()
    {
        return stateEventType;
    }
    
    public String toString()
    {
        String s = "state: ";
        if (stateEventType == SAVE_STATE)
        {
            s += "save -> "; 
        }
        else if (stateEventType == LOAD_STATE)
        {
            s += "restore <- "; 
        }
        else
        {
            s += "unknown operation <-> ";
        }
        return s += file;
    }
}
