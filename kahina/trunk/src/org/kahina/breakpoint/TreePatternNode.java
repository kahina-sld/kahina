package org.kahina.breakpoint;

import java.util.ArrayList;
import java.util.List;

public class TreePatternNode
{
    private TreeNodePattern pattern;
    
    private TreePatternNode parent;
    private List<TreePatternNode> children;  
    
    public TreePatternNode()
    {
        this.pattern = new TreeNodePattern();
        this.parent = null;
        this.children = new ArrayList<TreePatternNode>();
    }
    
    public TreePatternNode(TreeNodePattern pattern)
    {
        this.pattern = pattern;
        this.parent = null;
        this.children = new ArrayList<TreePatternNode>();
    }

    public List<TreePatternNode> getChildren()
    {
        return children;
    }

    public void setChildren(List<TreePatternNode> children)
    {
        this.children = children;
    }

    public TreePatternNode getParent()
    {
        return parent;
    }

    public void setParent(TreePatternNode parent)
    {
        this.parent = parent;
    }

    public TreeNodePattern getPattern()
    {
        return pattern;
    }

    public void setPattern(TreeNodePattern pattern)
    {
        this.pattern = pattern;
    }
}
