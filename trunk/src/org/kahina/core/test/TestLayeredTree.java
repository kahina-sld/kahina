package org.kahina.core.test;

import org.kahina.core.data.tree.KahinaMemTree;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class TestLayeredTree extends KahinaMemTree
{
	public TestLayeredTree()
	{
		super(new TestLayerDecider());
	}
    
    public static TestLayeredTree importXML(Document dom)
    {
        TestLayeredTree m = new TestLayeredTree();
        Element treeElement = dom.getDocumentElement();  
        NodeList childNodes = treeElement.getChildNodes();
        for (int i = 0; i < childNodes.getLength(); i++)
        {
            Node n = childNodes.item(i);
            if (n.getNodeName().equals("node"))
            {
                importXMLNode(m, (Element) n, -1);
                //TODO: a little risky, root node could be assigned another ID
                m.setRootID(0);
                break;
            }
        }
        return m;
    }
    
    private static void importXMLNode(TestLayeredTree m, Element node, int parentID)
    {
        int nodeID = 0;
        if (node.getAttribute("id").length() > 0)
        {
            nodeID = Integer.parseInt(node.getAttribute("id"));
        }
        else
        {
            nodeID = m.getNextFreeID();
        }
        m.nodeCaptions.put(nodeID, node.getAttribute("caption"));
        m.edgeLabels.put(nodeID, node.getAttribute("label"));
        if (node.getAttribute("status").length() > 0)
        {
            m.status.put(nodeID, Integer.parseInt(node.getAttribute("status")));
        }
        m.addChild(parentID, nodeID);
        //go through children recursively
        NodeList childNodes = node.getChildNodes();
        for (int i = 0; i < childNodes.getLength(); i++)
        {
            Node n = childNodes.item(i);
            if (n.getNodeName().equals("node"))
            {
                importXMLNode(m, (Element) n, nodeID);
            }
        }
    }
}
