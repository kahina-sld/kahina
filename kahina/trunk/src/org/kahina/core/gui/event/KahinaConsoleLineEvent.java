package org.kahina.core.gui.event;

import java.util.HashSet;
import java.util.Set;

import org.kahina.core.data.text.KahinaLineReference;
import org.kahina.core.event.KahinaEvent;

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
    
    public String toString()
    {
        return "console: lines " + consoleLines;
    }
}
