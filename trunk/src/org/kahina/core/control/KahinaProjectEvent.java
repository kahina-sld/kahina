package org.kahina.core.control;

import java.io.File;

public class KahinaProjectEvent extends KahinaEvent
{
    final KahinaProjectEventType projectEventType;
    //store project file for SAVE_PROJECT and LOAD_PROJECT, and program file for NEW_PROJECT
    final File file;
    
    public KahinaProjectEvent(KahinaProjectEventType projectEventType, File file)
    {
        super(KahinaEventTypes.PROJECT);
        this.projectEventType = projectEventType;
        this.file = file;
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
            s += "save -> "; 
        }
        else if (projectEventType == KahinaProjectEventType.LOAD_PROJECT)
        {
            s += "load <- "; 
        }
        else if (projectEventType == KahinaProjectEventType.NEW_PROJECT)
        {
            s += "new, grammar file: ";
        }
        else
        {
            s += "unknown operation <-> ";
        }
        return s += file;
    }
}
