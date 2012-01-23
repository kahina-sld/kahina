package org.kahina.core.gui.event;

import org.kahina.core.control.KahinaEvent;
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
    
    
    @Override
	public String toString()
    {
        return "message: " + line.toString();
    }
}
