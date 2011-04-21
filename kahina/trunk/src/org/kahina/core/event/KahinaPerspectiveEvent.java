package org.kahina.core.event;

import java.io.File;

public class KahinaPerspectiveEvent extends KahinaEvent
{
	File file;
    int perspectiveEventType;
    
    public static final int SAVE_PERSPECTIVE = 0;
    public static final int LOAD_PERSPECTIVE = 1;
    
    public KahinaPerspectiveEvent(int perspectiveEventType, File file)
    {
        super(KahinaEventTypes.PERSPECTIVE);
        this.perspectiveEventType = perspectiveEventType;
        this.file = file;
    }
    
    public File getFile()
    {
        return file;
    }
    
    public int getPerspectiveEventType()
    {
        return perspectiveEventType;
    }
    
    @Override
	public String toString()
    {
        String s = "perspective: ";
        if (perspectiveEventType == SAVE_PERSPECTIVE)
        {
            s += "save -> "; 
        }
        else if (perspectiveEventType == LOAD_PERSPECTIVE)
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
