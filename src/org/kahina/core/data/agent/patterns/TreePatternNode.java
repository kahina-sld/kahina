package org.kahina.core.data.agent.patterns;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.kahina.core.control.KahinaSimpleProperty;
import org.kahina.core.control.KahinaStepProperty;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class TreePatternNode implements Serializable
{
    /**
	 * 
	 */
	private static final long serialVersionUID = 8431727196172467206L;

	private KahinaSimpleProperty pattern;
    
    private TreePatternNode parent;
    private List<TreePatternNode> children;  
    
    public TreePatternNode()
    {
        this.pattern = new KahinaSimpleProperty();
        this.parent = null;
        this.children = new ArrayList<TreePatternNode>();
    }
    
    public TreePatternNode(KahinaSimpleProperty pattern)
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

    public KahinaSimpleProperty getPattern()
    {
        return pattern;
    }

    public void setPattern(KahinaSimpleProperty pattern)
    {
        this.pattern = pattern;
    }
    
    @Override
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
    
    public Element exportXML(Document dom)
    {
        Element nodePatternEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:pattern-node");
        nodePatternEl.appendChild(pattern.exportXML(dom));
        if (children.size() > 0)
        {
            Element childrenEl = dom.createElementNS("http://www.kahina.org/xml/kahina","kahina:pattern-node");
            for (TreePatternNode child : children)
            {
                childrenEl.appendChild(child.exportXML(dom));
            }
            nodePatternEl.appendChild(childrenEl);
        }
        return nodePatternEl;
    }
    
    public static TreePatternNode importXML(Element treePatternNodeNode)
    {
        TreePatternNode newTreePatternNode = new TreePatternNode();
        newTreePatternNode.pattern = KahinaSimpleProperty.importXML(treePatternNodeNode);
        NodeList childNodes = treePatternNodeNode.getChildNodes();
        for (int i = 0; i < childNodes.getLength(); i++)
        {
            if (childNodes.item(i).getNodeName().equals("children"))
            {
                NodeList childPatternNodeNodes = childNodes.item(i).getChildNodes();
                for (int j = 0; j < childPatternNodeNodes.getLength(); j++)
                {
                    if (childPatternNodeNodes.item(j).getNodeName().equals("patternNode"))
                    {      
                        TreePatternNode childPatternNode = TreePatternNode.importXML((Element) childPatternNodeNodes.item(j));
                        newTreePatternNode.addChild(childPatternNode);
                    }
                }
            }
        }
        return newTreePatternNode;
    }
}
