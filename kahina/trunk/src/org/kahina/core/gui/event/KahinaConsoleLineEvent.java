package org.kahina.core.gui.event;

import java.util.HashSet;
import java.util.Set;

import org.kahina.core.event.KahinaEvent;

public class KahinaConsoleLineEvent extends KahinaEvent
{
    Set<Integer> consoleLines;
    
    public KahinaConsoleLineEvent(Set<Integer> consoleLines)
    {
        super("console line");
        this.consoleLines = consoleLines;
    }
    
    public Set<Integer> getConsoleLines()
    {
        return consoleLines;
    }
    
    public String toString()
    {
        return "console: lines " + consoleLines;
    }
}
