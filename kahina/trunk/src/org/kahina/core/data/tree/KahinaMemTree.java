package org.kahina.core.data.tree;

import java.util.ArrayList;
import java.util.List;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class KahinaMemTree extends KahinaUnlayeredMemTree
{   
    public static final boolean verbose = false;
    
    public KahinaMemTree()
    {
    	this(new DefaultLayerDecider());
    }

    public KahinaMemTree(LayerDecider decider)
    {
        super(decider);
        setReferenceNode(super.getRootID(0));
    } 
    
    @Override
	public int getRootID(int layerID)
    {
        if (layerID == 0) return super.getRootID(0);
        int rootID = getReferenceNode();
        while (decider.decideOnLayer(rootID, this) >= layerID)
        {
            rootID = super.getParent(rootID,0);
        }
        return rootID;
    }
    
    /**
     * Returns the lowest ancestor of nodeID whose layer is lower than or equals
     * layerID.
     */
    @Override
	public int getParent(int nodeID, int layerID)
    {
        if (nodeID == getRootID(layerID)) return -1;
        int parent = super.getParent(nodeID,0);
        while (decider.decideOnLayer(parent, this) > layerID)
        {
            parent = super.getParent(parent,0);
        }
        //System.err.println("Determined parent for node " + nodeID + ": " + parent);
        return parent;
    }
    
    /**
     * Returns those descendants of nodeID whose layer is lower than or equals
     * layerID and which are not dominated by any other such descendant - but
     * only if the layer of nodeID is greater than or equals layerID OR nodeID
     * is the root of the tree fragment currently being drawn as indicated by
     * referenceNode (otherwise returns the empty list).
     */
    @Override
	public List<Integer> getChildren(int nodeID, int layerID)
    {
        if (verbose) System.err.print("KahinaLayeredTree.getChildren(" + nodeID + "," + layerID + ") = ");
        List<Integer> chi = new ArrayList<Integer>();
        List<Integer> frontLine = new ArrayList<Integer>();
        if (nodeID == getRootID(layerID) || decider.decideOnLayer(nodeID, this) >= layerID)
        {
            frontLine.addAll(super.getChildren(nodeID, layerID));
        }
        if (verbose) System.err.println("front line: " + frontLine);
        while (frontLine.size() > 0)
        {
            int child = frontLine.remove(0);
            if (verbose) System.err.println("child: " + child + " level: " + decider.decideOnLayer(child, this));
            if (decider.decideOnLayer(child, this) <= layerID)
            {
                chi.add(child);
            }
            else
            {
            	// TODO ke: Shouldn't these children be added at the beginning
            	// of the front line? This seems to risk getting the order
            	// wrong.
                frontLine.addAll(super.getChildren(child,0));
            }
        }
        if (verbose) System.err.println(" node: " + nodeID + " layer: " + layerID + " chi: " + chi);
        //System.err.println(chi);
        return chi;
    }
    
    public static KahinaTree importXML(Document dom)
    {
        KahinaMemTree m = new KahinaMemTree();
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
    
    private static void importXMLNode(KahinaMemTree m, Element node, int parentID)
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
