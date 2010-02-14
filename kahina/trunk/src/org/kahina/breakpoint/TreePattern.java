package org.kahina.breakpoint;

public class TreePattern
{
    private TreePatternNode root;
    
    public TreePattern()
    {
        root = new TreePatternNode();
    }
    
    public TreePattern(TreeNodePattern rootPattern)
    {
        root = new TreePatternNode(rootPattern);
    }

    public TreePatternNode getRoot()
    {
        return root;
    }

    public void setRoot(TreePatternNode root)
    {
        this.root = root;
    }
}
