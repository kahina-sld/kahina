package org.kahina.data.tree;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;

import org.kahina.core.data.KahinaObject;
import org.kahina.data.chart.KahinaChart;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class KahinaTree extends KahinaObject
{
    static int lastID = 0;
    static String type = "KahinaTree";
    
    //empty tree has rootID = -1
    protected int rootID = -1;
    
    //encode properties of individual nodes
    protected HashMap<Integer, Integer> parents;
    protected HashMap<Integer, List<Integer>> children;
    protected HashMap<Integer, String> nodeCaptions; //captions are displayed on the nodes
    protected HashMap<Integer, String> edgeLabels; //labels are displayed on the edges to the parent
    protected HashMap<Integer, Integer> status; //appearance of nodes can be steered by appearance
    protected HashSet<Integer> collapsed; //node collapsing is stored in the model, not in individual views!
    
    HashSet<Integer> terminals; //terminals will be displayed on one level
    
    //store the ID of the next node that is going to be added
    private int nextID = 0;
    //internal link to the primary model, must sometimes be used for decisions
    protected KahinaTree primaryModel;
    
    public KahinaTree()
    {
        super(lastID++);
        
        rootID = -1;
        
        parents = new HashMap<Integer, Integer>();
        children = new HashMap<Integer, List<Integer>>();
        nodeCaptions = new HashMap<Integer, String>();
        edgeLabels = new HashMap<Integer, String>();
        status = new HashMap<Integer, Integer>();
        collapsed = new HashSet<Integer>();
        
        terminals = new HashSet<Integer>();
        
        primaryModel = this;
    }
    
    public void setPrimaryModel(KahinaTree primaryModel)
    {
        this.primaryModel = primaryModel;
    }
    
    public int getRootID(int layerID)
    {
        return rootID;
    }
    
    public void setRootID(int rootID)
    {
        this.rootID = rootID;
    }
    
    public void addChild(int parent, int child)
    {
        if (parent != -1)
        {
            List<Integer> childIDs = children.get(parent);
            if (childIDs == null)
            {
                childIDs = new ArrayList<Integer>();
                children.put(parent, childIDs);
            }
            childIDs.add(child);
        }
        parents.put(child, parent);
    }
    
    public int getParent(int nodeID, int layerID)
    {
        Integer parent = parents.get(nodeID);
        if (parent == null) return -1;
        return parent;
    }
    
    public String getNodeCaption(int nodeID)
    {
        String caption = nodeCaptions.get(nodeID);
        if (caption == null)
        {
            return null;
        }
        else
        {
            return caption;
        }
    }
    
    public String getEdgeLabel(int nodeID)
    {
        String label = edgeLabels.get(nodeID);
        if (label == null)
        {
            return "";
        }
        else
        {
            return label;
        }
    }
    
    public int getNodeStatus(int nodeID)
    {
        Integer st = status.get(nodeID);
        if (st == null)
        {
            return 0;
        }
        else
        {
            return st;
        }
    }
    
    public List<Integer> getChildren(int nodeID, int layerID)
    {
        //System.err.print("KahinaTree.getChildren(" + nodeID + "," + layerID + ") = ");
        List<Integer> ids = children.get(nodeID);
        if (ids == null)
        {
            //System.err.println("[]");
            return new ArrayList<Integer>();
        }
        else
        {
            //System.err.println(ids);
            return ids;
        }
    }
    
    public List<Integer> getLeaves()
    {
        List<Integer> leaves = new LinkedList<Integer>();
        collectLeaves(rootID, leaves);
        return leaves;
    }
    
    private void collectLeaves(int nodeID, List<Integer> leaves)
    {
        if (nodeID != -1)
        {
            List<Integer> nodeChildren = children.get(nodeID);
            if (nodeChildren == null)
            {
                leaves.add(nodeID);
            }
            else
            {
                for (int child : nodeChildren)
                {
                    collectLeaves(child, leaves);
                }
            }
        }
    }
    
    public HashMap<Integer, String> getNodeCaptions()
    {
        return nodeCaptions;
    }

    public void setNodeCaptions(HashMap<Integer, String> nodeCaptions)
    {
        this.nodeCaptions = nodeCaptions;
    }

    public HashMap<Integer, String> getEdgeLabels()
    {
        return edgeLabels;
    }

    public void setEdgeLabels(HashMap<Integer, String> edgeLabels)
    {
        this.edgeLabels = edgeLabels;
    }

    public HashMap<Integer, Integer> getStatus()
    {
        return status;
    }

    public void setStatus(HashMap<Integer, Integer> status)
    {
        this.status = status;
    }
    
    public boolean isCollapsed(int nodeID)
    {
        return collapsed.contains(nodeID);
    }
    
    public void collapse(int nodeID)
    {
        if (nodeID != -1)
        {
            collapsed.add(nodeID);
        }
    }
    
    public void decollapse(int nodeID)
    {
        collapsed.remove(nodeID);
    }
    
    public void decollapseAll()
    {
        collapsed = new HashSet<Integer>();
    }
    
    public void toggleCollapse(int nodeID)
    {
        if (!isCollapsed(nodeID))
        {
            collapse(nodeID);
        }
        else
        {
            decollapse(nodeID);
        }
    }
    
    public boolean hasCollapsedAncestor(int nodeID)
    {
        Integer parent = parents.get(nodeID);
        while (parent != null)
        {
            if (isCollapsed(parent))
            {
                return true;
            }
            parent = parents.get(parent);
        }
        return false;
    }
    
    public int addNode(String caption, String label, int nodeStatus)
    {
        int nodeID = getNextFreeID();
        nodeCaptions.put(nodeID, caption);
        edgeLabels.put(nodeID, label);
        status.put(nodeID,nodeStatus);
        return nodeID;
    }

    protected int getNextFreeID()
    {
        int nextIDHyp = nextID;
        while (parents.get(nextIDHyp) != null)
        {
            nextIDHyp++;
        }
        nextID = nextIDHyp + 1;
        return nextIDHyp;
    }
    
    public void clear()
    {
        rootID = -1;
        
        parents = new HashMap<Integer, Integer>();
        children = new HashMap<Integer, List<Integer>>();
        nodeCaptions = new HashMap<Integer, String>();
        edgeLabels = new HashMap<Integer, String>();
        status = new HashMap<Integer, Integer>();
        collapsed = new HashSet<Integer>();
        
        terminals = new HashSet<Integer>();
        
        primaryModel = this;
        
        nextID = 0;
    }
    
    public String exportXML()
    {
        StringBuilder b = new StringBuilder("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n");
        b.append("<kahinaTree>\n");
        if (rootID != -1)
        {
            exportXML(b, rootID, 2);
        }
        b.append("</kahinaTree>\n");
        return b.toString();
    }
    
    private void exportXML(StringBuilder b, int node, int depth)
    {
        createSpace(b, depth);
        b.append("<node id=\"" + node + "\" caption=\"" + getNodeCaption(node) + "\" label=\"" + getEdgeLabel(node) + "\" status=\"" + getNodeStatus(node) + "\">\n");
        for (int child : getChildren(node, 0))
        {
            exportXML(b, child, depth + 2);
        }
        createSpace(b, depth);
        b.append("</node>\n");
    }
    
    private void createSpace(StringBuilder b, int amount)
    {
        for (int i = 0; i < amount; i++)
        {
            b.append(' ');
        }
    }
    
    public static KahinaTree importXML(Document dom)
    {
        KahinaTree m = new KahinaTree();
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
    
    private static void importXMLNode(KahinaTree m, Element node, int parentID)
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
