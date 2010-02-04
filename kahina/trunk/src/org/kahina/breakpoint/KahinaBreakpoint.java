package org.kahina.breakpoint;

import java.awt.Color;

public class KahinaBreakpoint
{
    boolean active;
    Color signalColor;
    
    public TreeAutomaton compile()
    {
        return new TreeAutomaton();
    }
    
    public void activate()
    {
        active = true;
    }
    
    public void deactivate()
    {
        active = false;
    }
}
