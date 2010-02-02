package org.kahina.gui.breakpoint;

import java.io.File;

import org.kahina.control.event.KahinaEvent;

public class BreakpointEditorEvent extends KahinaEvent
{    
    File file;
    int stateEventType;
    
    int goalID;
    
    public static final int NEW_BREAKPOINT = 0;
    public static final int EXPORT_BREAKPOINT = 1;
    public static final int IMPORT_BREAKPOINT = 2;
    public static final int ACTIVATE_BREAKPOINT = 3;
    public static final int DEACTIVATE_BREAKPOINT = 4;
    public static final int REMOVE_BREAKPOINT = 5;
    
    public BreakpointEditorEvent(int stateEventType)
    {
        super("breakpoint_editor");
        this.stateEventType = stateEventType;
    }
    
    public BreakpointEditorEvent(int stateEventType, File file)
    {
        super("breakpoint_editor");
        this.stateEventType = stateEventType;
        this.file = file;
    }
    
    public BreakpointEditorEvent(int stateEventType, int goalID)
    {
        super("breakpoint_editor");
        this.stateEventType = stateEventType;
        this.goalID = goalID;
    }
    
    public File getFile()
    {
        return file;
    }
    
    public int getStateEventType()
    {
        return stateEventType;
    }
    
    public int getGoalID()
    {
        return goalID;
    }
    
    public String toString()
    {
        String s = "breakpoint: ";
        if (stateEventType == NEW_BREAKPOINT)
        {
            s += "new "; 
        }
        else if (stateEventType == EXPORT_BREAKPOINT)
        {
            s += "export -> "; 
        }
        else if (stateEventType == IMPORT_BREAKPOINT)
        {
            s += "import <- "; 
        }
        else if (stateEventType == ACTIVATE_BREAKPOINT)
        {
            s += "activate " + goalID; 
        }
        else if (stateEventType == DEACTIVATE_BREAKPOINT)
        {
            s += "deactivate " + goalID; 
        }
        else if (stateEventType == REMOVE_BREAKPOINT)
        {
            s += "remove " + goalID; 
        }
        else
        {
            s += "unknown operation <-> ";
        }
        return s += file;
    }
}
