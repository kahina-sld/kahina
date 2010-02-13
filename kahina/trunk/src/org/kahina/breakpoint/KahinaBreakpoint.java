package org.kahina.breakpoint;

import java.awt.Color;

public class KahinaBreakpoint
{
    private String name;
    private boolean active;
    private Color signalColor;
    private TreePattern pattern;
    
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

    public TreePattern getPattern()
    {
        return pattern;
    }

    public void setPattern(TreePattern pattern)
    {
        this.pattern = pattern;
    }
}
