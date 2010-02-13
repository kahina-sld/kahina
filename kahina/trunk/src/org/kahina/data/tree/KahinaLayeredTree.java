package org.kahina.data.tree;

import java.util.ArrayList;
import java.util.List;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class KahinaLayeredTree extends KahinaTree
{   
    //reference node should always be the same across views, so store it here
    int referenceNode;

    public KahinaLayeredTree()
    {
        super();
        referenceNode = super.getRootID(0);
    } 
    
    public int getReferenceNode()
    {
        return referenceNode;
    }

    public void setReferenceNode(int referenceNode)
    {
        this.referenceNode = referenceNode;
    }
    
    public int getRootID(int layerID)
    {
        if (layerID == 0) return super.getRootID(0);
        int rootID = referenceNode;
        while (decideOnLevel(rootID) >= layerID)
        {
            rootID = super.getParent(rootID,0);
        }
        return rootID;
    }
    
    public int getParent(int nodeID, int layerID)
    {
        if (nodeID == getRootID(layerID)) return -1;
        int parent = super.getParent(nodeID,0);
        while (decideOnLevel(parent) > layerID)
        {
            parent = super.getParent(parent,0);
        }
        //System.err.println("Determined parent for node " + nodeID + ": " + parent);
        return parent;
    }
    
    public List<Integer> getChildren(int nodeID, int layerID)
    {
        //System.err.print("KahinaLayeredTree.getChildren(" + nodeID + "," + layerID + ") = ");
        List<Integer> chi = new ArrayList<Integer>();
        List<Integer> frontLine = new ArrayList<Integer>();
        if (nodeID == getRootID(layerID) || decideOnLevel(nodeID) >= layerID)
        {
            frontLine.addAll(super.getChildren(nodeID, layerID));
        }
        //System.err.println("front line: " + frontLine);
        while (frontLine.size() > 0)
        {
            int child = frontLine.remove(0);
            //System.err.println("child: " + child + " level: " + decideOnLevel(child));
            if (decideOnLevel(child) <= layerID)
            {
                chi.add(child);
            }
            else
            {
                frontLine.addAll(super.getChildren(child,0));
            }
        }
        //System.err.println(" node: " + nodeID + " layer: " + layerID + " chi: " + chi);
        //System.err.println(chi);
        return chi;
    }
    
    //this method should be implemented by deriving classes!
    //full access to the tree model, can thus be based on node info, tree structure, status information
    //default version: all the nodes belong to level 0
    public int decideOnLevel(int nodeID)
    {
        return 0;
    }
    
    public static KahinaLayeredTree importXML(Document dom)
    {
        KahinaLayeredTree m = new KahinaLayeredTree();
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
    
    private static void importXMLNode(KahinaLayeredTree m, Element node, int parentID)
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
