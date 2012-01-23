package org.kahina.core.control;

import java.io.File;


public class KahinaSessionEvent extends KahinaEvent
{
    File file;
    int sessionEventType;
    
    public static final int SAVE_SESSION = 0;
    public static final int LOAD_SESSION = 1;
    
    public KahinaSessionEvent(int sessionEventType, File file)
    {
        super(KahinaEventTypes.SESSION);
        this.sessionEventType = sessionEventType;
        this.file = file;
    }
    
    public File getFile()
    {
        return file;
    }
    
    public int getSessionEventType()
    {
        return sessionEventType;
    }
    
    @Override
	public String toString()
    {
        String s = "state: ";
        if (sessionEventType == SAVE_SESSION)
        {
            s += "save -> "; 
        }
        else if (sessionEventType == LOAD_SESSION)
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
