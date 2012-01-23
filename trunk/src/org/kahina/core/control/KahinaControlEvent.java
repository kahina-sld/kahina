package org.kahina.core.control;


public class KahinaControlEvent extends KahinaEvent
{
    private String command;
    
    private Object[] arguments;
    
    private static final Object[] NOARGS = new Object[0];
    
    public KahinaControlEvent(String command)
    {
    	this(command, NOARGS);
    }
    
    public KahinaControlEvent(String command, Object[] arguments)
    {
        super(KahinaEventTypes.CONTROL);
        this.command = command;
        this.arguments = arguments;
    }
    
    public String getCommand()
    {
        return command;
    }
    
    public Object[] getArguments()
    {
    	return arguments;
    }
    
    @Override
	public String toString()
    {
        return "control: " + command;
    }
}
