package org.kahina.control.event;

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
