package org.kahina.core.breakpoint;

import java.awt.Color;

public class KahinaBreakpoint
{
    static int number = 0;
    private String name;
    private boolean active;
    private Color signalColor;
    private TreePattern pattern;
    //has one of the constant values in KahinaBreakpointType
    private int type;
    
    public KahinaBreakpoint(int type)
    {
        number++;
        setName("Breakpoint " + number);
        signalColor = randomColor();
        active = true;
        pattern = new TreePattern();
        this.type = type;
    }
    
    public TreeAutomaton compile()
    {
        TreeAutomaton a = new TreeAutomaton(this);
        int rootState = compileNode(a, pattern.getRoot());
        a.acceptingStates.add(rootState);
        return a;
    }
    
    private int compileNode(TreeAutomaton a, TreePatternNode node)
    {
        int state = a.states.size();
        a.states.add(state);
        TreeAutomatonRule rule = new TreeAutomatonRule();
        rule.assignedLabel = state;
        rule.pattern = node.getPattern();
        for (TreePatternNode child : node.getChildren())
        {
            rule.requiredChildAnnotations.add(compileNode(a, child));
        }
        a.rules.add(rule);
        return state;
    }
    
    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name;
    }
    
    public boolean isActive()
    {
        return active;
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
        if (active)
        {
            return name;
        }
        else
        {
            return "#" + name;
        }
    }

    public TreePattern getPattern()
    {
        return pattern;
    }

    public void setPattern(TreePattern pattern)
    {
        this.pattern = pattern;
    }
    
    public Color randomColor()
    {
        int r = (int) (Math.random() * 256);
        int g = (255 - r) + (int) (Math.random() * r);
        int b = 510 - r - g;
        return new Color(r,g,b);
    }

    public int getType()
    {
        return type;
    }

    public void setType(int type)
    {
        this.type = type;
    }
}
