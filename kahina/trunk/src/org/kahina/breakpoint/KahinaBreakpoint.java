package org.kahina.breakpoint;

import java.awt.Color;

public class KahinaBreakpoint
{
    private String name;
    private boolean active;
    private Color signalColor;
    
    public TreeAutomaton compile()
    {
        return new TreeAutomaton();
    }
    
    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name;
    }
    
    public void activate()
    {
        active = true;
    }
    
    public void deactivate()
    {
        active = false;
    }

    public Color getSignalColor()
    {
        return signalColor;
    }

    public void setSignalColor(Color signalColor)
    {
        this.signalColor = signalColor;
    }
    
    public String toString()
    {
        return name;
    }
}
