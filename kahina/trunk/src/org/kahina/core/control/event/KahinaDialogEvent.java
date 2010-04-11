package org.kahina.core.control.event;

public class KahinaDialogEvent extends KahinaEvent
{
    int dialogEventType;
    
    public static final int BREAKPOINTS = 0;
    public static final int PARSE_OPTIONS = 1;
    public static final int HELP = 2;
    public static final int ABOUT = 3;
    
    public KahinaDialogEvent(int dialogEventType)
    {
        super("dialog");
        this.dialogEventType = dialogEventType;
    }
    
    public int getDialogEventType()
    {
        return dialogEventType;
    }
    
    public String toString()
    {
        String s = "dialog: ";
        if (dialogEventType == BREAKPOINTS)
        {
            s += "breakpoints"; 
        }
        else if (dialogEventType == PARSE_OPTIONS)
        {
            s += "parse options"; 
        }
        else if (dialogEventType == HELP)
        {
            s += "help"; 
        }
        else if (dialogEventType == ABOUT)
        {
            s += "about"; 
        }
        else
        {
            s += "unknown dialog";
        }
        return s;
    }
}
