package org.kahina.test;

import org.kahina.data.tree.KahinaLayeredTree;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class TestLayeredTree extends KahinaLayeredTree
{
    //this method should be implemented by deriving classes!
    //full access to the tree model, can thus be based on node info, tree structure, status information
    //TODO: in the case of the secondary tree model, use the captions from the primary model instead!
    public int decideOnLevel(int nodeID)
    {
        if (nodeID == getRootID(0)) return 0;
        if (nodeID == -1) return -1;
        if (primaryModel.getNodeCaption(nodeID).indexOf("rule") != -1 || primaryModel.getNodeCaption(nodeID).indexOf("\"") != -1) return 0;
        else if (primaryModel.getNodeCaption(nodeID).indexOf("goal") != -1) return 1;
        return 2;
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
                m.rootID = 0;
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
