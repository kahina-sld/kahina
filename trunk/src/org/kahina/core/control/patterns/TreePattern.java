package org.kahina.core.control.patterns;

import java.io.Serializable;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class TreePattern implements Serializable
{
    /**
	 * 
	 */
	private static final long serialVersionUID = -3784645758172474513L;
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
    
    @Override
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
    
    public static TreePattern importXML(Element treePatternNode)
    {
        TreePattern newTreePattern = new TreePattern();
        //last defined TreePatternNode will become the root node, possible others will be discarded
        NodeList childNodes = treePatternNode.getChildNodes();
        for (int i = 0; i < childNodes.getLength(); i++)
        {
            if (childNodes.item(i).getNodeName().equals("patternNode"))
            {
                newTreePattern.root = TreePatternNode.importXML((Element) childNodes.item(i));
            }
        }
        return newTreePattern;
    }
}
