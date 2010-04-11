package org.kahina.core.control.event;

public class KahinaControlEvent extends KahinaEvent
{
    String command;
    
    public KahinaControlEvent(String command)
    {
        super("control");
        this.command = command;
    }
    
    public String getCommand()
    {
        return command;
    }
    
    public String toString()
    {
        return "control: " + command;
    }
}
