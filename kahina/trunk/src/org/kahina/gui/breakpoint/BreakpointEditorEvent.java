package org.kahina.gui.breakpoint;

import java.io.File;

import org.kahina.control.event.KahinaEvent;

public class BreakpointEditorEvent extends KahinaEvent
{    
    File file;
    int editorEventType;
    
    int goalID;
    
    public static final int NEW_BREAKPOINT = 0;
    public static final int EXPORT_BREAKPOINT = 1;
    public static final int IMPORT_BREAKPOINT = 2;
    public static final int ACTIVATE_BREAKPOINT = 3;
    public static final int DEACTIVATE_BREAKPOINT = 4;
    public static final int REMOVE_BREAKPOINT = 5;
    public static final int CHANGE_NODE_SELECTION_MODE = 6;
    
    public BreakpointEditorEvent(int editorEventType)
    {
        super("breakpoint_editor");
        this.editorEventType = editorEventType;
    }
    
    public BreakpointEditorEvent(int editorEventType, File file)
    {
        super("breakpoint_editor");
        this.editorEventType = editorEventType;
        this.file = file;
    }
    
    public BreakpointEditorEvent(int editorEventType, int goalID)
    {
        super("breakpoint_editor");
        this.editorEventType = editorEventType;
        this.goalID = goalID;
    }
    
    public File getFile()
    {
        return file;
    }
    
    public int getEditorEventType()
    {
        return editorEventType;
    }
    
    public int getGoalID()
    {
        return goalID;
    }
    
    public String toString()
    {
        String s = "breakpoint: ";
        if (editorEventType == NEW_BREAKPOINT)
        {
            s += "new "; 
        }
        else if (editorEventType == EXPORT_BREAKPOINT)
        {
            s += "export -> "; 
        }
        else if (editorEventType == IMPORT_BREAKPOINT)
        {
            s += "import <- "; 
        }
        else if (editorEventType == ACTIVATE_BREAKPOINT)
        {
            s += "activate " + goalID; 
        }
        else if (editorEventType == DEACTIVATE_BREAKPOINT)
        {
            s += "deactivate " + goalID; 
        }
        else if (editorEventType == REMOVE_BREAKPOINT)
        {
            s += "remove " + goalID; 
        }
        else if (editorEventType == CHANGE_NODE_SELECTION_MODE)
        {
            s += "selection mode " + goalID; 
        }
        else
        {
            s += "unknown operation <-> ";
        }
        return s += file;
    }
}
