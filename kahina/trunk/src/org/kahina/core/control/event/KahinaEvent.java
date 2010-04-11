package org.kahina.core.control.event;

public class KahinaEvent
{
    String type;
    
    public KahinaEvent(String type)
    {
        this.type = type;
    }
    
    public String getType()
    {
        return type;
    }
    
    public String toString()
    {
        return type;
    }
}
