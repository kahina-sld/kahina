package org.kahina.core.control;

import java.io.File;

public class KahinaProjectEvent extends KahinaEvent
{
    final KahinaProjectEventType projectEventType;
    //store project name for NEW_PROJECT
    final String name;
    //store project file for SAVE_PROJECT and LOAD_PROJECT, and program file for NEW_PROJECT
    final File file;
    //store selected menu item index for LOAD_DEFAULT_PROJECT and LOAD_RECENT_PROJECT
    final int id;
    
    public KahinaProjectEvent(KahinaProjectEventType projectEventType, File file, String name)
    {
        super(KahinaEventTypes.PROJECT);
        this.projectEventType = projectEventType;
        this.name = name;
        this.file = file;
        this.id = -1;
    }
    
    public KahinaProjectEvent(KahinaProjectEventType projectEventType, int id)
    {
        super(KahinaEventTypes.PROJECT);
        this.projectEventType = projectEventType;
        this.name = "";
        this.file = new File("");
        this.id = id;
    }
    
    public String getName()
    {
        return name;
    }
    
    public int getID()
    {
        return id;
    }
    
    public File getFile()
    {
        return file;
    }
    
    public KahinaProjectEventType getProjectEventType()
    {
        return projectEventType;
    }
    
    @Override
    public String toString()
    {
        String s = "project: ";
        if (projectEventType == KahinaProjectEventType.SAVE_PROJECT)
        {
            s += "save -> " + file; 
        }
        else if (projectEventType == KahinaProjectEventType.LOAD_PROJECT)
        {
            s += "load <- " + file; 
        }
        else if (projectEventType == KahinaProjectEventType.NEW_PROJECT)
        {
            s += "new, grammar file: " + file;
        }
        else if (projectEventType == KahinaProjectEventType.LOAD_RECENT_PROJECT)
        {
            s += "load recent project #" + id; 
        }
        else if (projectEventType == KahinaProjectEventType.LOAD_DEFAULT_PROJECT)
        {
            s += "load predefined project #" + id; 
        }
        else
        {
            s += "unknown operation <-> " + id + ", " + file;
        }
        return s;
    }
}
