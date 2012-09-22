package org.kahina.core.gui.event;

import java.io.File;

import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;

public class KahinaPerspectiveEvent extends KahinaEvent
{
	File file;
    int perspectiveEventType;
    int id;
    
    public static final int SAVE_PERSPECTIVE = 0;
    public static final int LOAD_PERSPECTIVE = 1;
    public static final int LOAD_RECENT_PERSPECTIVE = 2;
    public static final int LOAD_DEFAULT_PERSPECTIVE = 3;
    
    public KahinaPerspectiveEvent(int perspectiveEventType, int id)
    {
        super(KahinaEventTypes.PERSPECTIVE);
        this.perspectiveEventType = perspectiveEventType;
        this.id = id;
    }
    
    public KahinaPerspectiveEvent(int perspectiveEventType, File file)
    {
        super(KahinaEventTypes.PERSPECTIVE);
        this.perspectiveEventType = perspectiveEventType;
        this.file = file;
    }
    
    public int getID()
    {
    	return id;
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
            s += "save -> " + file; 
        }
        else if (perspectiveEventType == LOAD_PERSPECTIVE)
        {
            s += "load <- " + file; 
        }
        else if (perspectiveEventType == LOAD_RECENT_PERSPECTIVE)
        {
            s += "load recent perspective #" + id; 
        }
        else if (perspectiveEventType == LOAD_DEFAULT_PERSPECTIVE)
        {
            s += "load predefined perspective #" + id; 
        }
        else
        {
            s += "unknown operation <-> " + id + ", " + file;
        }
        return s;
    }
}
