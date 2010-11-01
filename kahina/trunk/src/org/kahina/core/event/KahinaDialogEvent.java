package org.kahina.core.event;

public class KahinaDialogEvent extends KahinaEvent
{
    
    public static final int PARSE_OPTIONS = 1;
    public static final int HELP = 2;
    public static final int ABOUT = 3;
    public static final int PRIMARY_BREAKPOINTS = 4;
    public static final int SECONDARY_BREAKPOINTS = 5;
    public static final int SKIP_POINTS = 6;
    public static final int CREEP_POINTS = 7;
    public static final int FAIL_POINTS = 8;
	public static final int FULL_PROFILE = 9;
	public static final int CALL_SUBTREE_PROFILE = 10;
	public static final int SEARCH_SUBTREE_PROFILE = 11;
	public static final int EDIT_WARNINGS = 12;
	public static final int PARSE = 13;
	public static final int COMPILE = 14;
	
	private static final Object[] NOARGS = new Object[0];
	
    private int dialogEventType;
    
    private Object[] arguments;
    
    public KahinaDialogEvent(int dialogEventType)
    {
    	this(dialogEventType, NOARGS);
    }
    
    public KahinaDialogEvent(int dialogEventType, Object[] arguments)
    {
        super(KahinaEventTypes.DIALOG);
        this.dialogEventType = dialogEventType;
        this.arguments = arguments;
    }
    
    public int getDialogEventType()
    {
        return dialogEventType;
    }
    
    public Object[] getArguments()
    {
    	return arguments;
    }
    
    @Override
	public String toString()
    {
        String s = "dialog: ";
        if (dialogEventType == PARSE_OPTIONS)
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
        else if (dialogEventType == PRIMARY_BREAKPOINTS)
        {
            s += "primary breakpoints"; 
        }
        else if (dialogEventType == SECONDARY_BREAKPOINTS)
        {
            s += "secondary breakpoints"; 
        }
        else if (dialogEventType == SKIP_POINTS)
        {
            s += "skip points"; 
        }
        else if (dialogEventType == CREEP_POINTS)
        {
            s += "creep points"; 
        }
        else if (dialogEventType == FAIL_POINTS)
        {
            s += "fail points"; 
        }
        else
        {
            s += "unknown dialog";
        }
        return s;
    }
}
