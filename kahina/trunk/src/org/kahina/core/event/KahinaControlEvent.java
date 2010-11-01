package org.kahina.core.event;

public class KahinaControlEvent extends KahinaEvent
{
    private String command;
    
    private String[] arguments;
    
    private static final String[] NOARGS = new String[0];
    
    public KahinaControlEvent(String command)
    {
    	this(command, NOARGS);
    }
    
    public KahinaControlEvent(String command, String[] arguments)
    {
        super(KahinaEventTypes.CONTROL);
        this.command = command;
        this.arguments = arguments;
    }
    
    public String getCommand()
    {
        return command;
    }
    
    public String[] getArguments()
    {
    	return arguments;
    }
    
    @Override
	public String toString()
    {
        return "control: " + command;
    }
}
