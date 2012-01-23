package org.kahina.core.control;

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
    
    @Override
	public String toString()
    {
        return type;
    }
}
