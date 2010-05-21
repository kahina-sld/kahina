package org.kahina.core.breakpoint;

import java.util.ArrayList;
import java.util.List;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

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
    
    public void addChild(TreePatternNode child)
    {
        children.add(child);
        child.setParent(this);
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
    
    public String toString()
    {
        StringBuilder str = new StringBuilder(pattern.toString());
        if (!children.isEmpty())
        {
            str.append(children);
        }
        return str.toString();   
    }
    
    public String exportXML(boolean asFile)
    {
        StringBuilder b = new StringBuilder("");
        if (asFile) b.append("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n");
        b.append("<patternNode>\n");
        b.append(pattern.exportXML(false));
        if (children.size() > 0)
        {
            b.append("<children>\n");
            for (TreePatternNode child : children)
            {
                b.append(child.exportXML(false));
            }
            b.append("</children>\n");
        }
        b.append("</patternNode>");
        return b.toString();
    }
    
    public static TreePatternNode importXML(Element treePatternNodeNode)
    {
        System.err.println("Importing TreePatternNode!");
        TreePatternNode newTreePatternNode = new TreePatternNode();
        NodeList childNodes = treePatternNodeNode.getChildNodes();
        for (int i = 0; i < childNodes.getLength(); i++)
        {
            if (childNodes.item(i).getNodeName().equals("pattern"))
            {
                newTreePatternNode.pattern = TreeNodePattern.importXML((Element) childNodes.item(i));
            }
            if (childNodes.item(i).getNodeName().equals("children"))
            {
                NodeList childPatternNodeNodes = treePatternNodeNode.getChildNodes();
                for (int j = 0; j < childPatternNodeNodes.getLength(); j++)
                {
                    if (childPatternNodeNodes.item(i).getNodeName().equals("patternNode"))
                    {
                        TreePatternNode childPatternNode = TreePatternNode.importXML((Element) childPatternNodeNodes.item(i));
                        newTreePatternNode.addChild(childPatternNode);
                    }
                }
            }
        }
        return newTreePatternNode;
    }
}
