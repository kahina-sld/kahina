package org.kahina.core.gui.event;

import java.util.Set;

import org.kahina.core.control.KahinaEvent;
import org.kahina.core.data.text.KahinaLineReference;

public class KahinaConsoleLineEvent extends KahinaEvent
{
    Set<KahinaLineReference> consoleLines;
    
    public KahinaConsoleLineEvent(Set<KahinaLineReference> consoleLines)
    {
        super("console line");
        this.consoleLines = consoleLines;
    }
    
    public Set<KahinaLineReference> getConsoleLines()
    {
        return consoleLines;
    }
    
    @Override
	public String toString()
    {
        return "console: lines " + consoleLines;
    }
}
