package org.kahina.core.event;

import org.kahina.core.data.text.KahinaLineReference;

public class KahinaMessageEvent extends KahinaEvent
{
    KahinaLineReference line;
    
    public KahinaMessageEvent(KahinaLineReference line)
    {
        super("message");
        this.line = line;
    }
    
    public KahinaLineReference getLine()
    {
        return line;
    }
    
    
    public String toString()
    {
        return "message: " + line.toString();
    }
}
