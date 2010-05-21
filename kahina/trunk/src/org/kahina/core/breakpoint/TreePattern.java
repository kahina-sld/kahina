package org.kahina.core.breakpoint;

import org.kahina.core.io.color.ColorIO;

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
    
    public String toString()
    {
        return root.toString();
    }
    
    public String exportXML(boolean asFile)
    {
        StringBuilder b = new StringBuilder("");
        if (asFile) b.append("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n");
        b.append("<treePattern>\n");
        b.append(root.exportXML(false));
        b.append("</treePattern>");
        return b.toString();
    }
}
