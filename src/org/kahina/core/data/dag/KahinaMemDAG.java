package org.kahina.core.data.dag;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class KahinaMemDAG extends KahinaDAG
{
    /**
	 * 
	 */
	private static final long serialVersionUID = -7505557494606184493L;
	
	protected Set<Integer> roots;
	
	//encode properties of individual nodes
    protected Map<Integer, List<Integer>> incomingEdges;
    protected Map<Integer, List<Integer>> outgoingEdges;
    protected Map<Integer, String> nodeCaptions; //captions are displayed on the nodes
    protected Map<Integer, Integer> status; //appearance of nodes can be steered by appearance
    protected Set<Integer> collapsed; //node collapsing is stored in the model, not in individual views!
    
    //encode properties of individualEdges
    protected Map<Integer, String> edgeLabels; //labels are displayed on the edges to the parent
    protected Map<Integer, Integer> startNodes;
    protected Map<Integer, Integer> endNodes;
    
    //store the ID of the next node and the next edge that is going to be added
    protected int nextNodeID = 0;
    protected int nextEdgeID = 0;
    
    public KahinaMemDAG()
    {        
        super();
        
        roots = new HashSet<Integer>();
        
        incomingEdges = new HashMap<Integer, List<Integer>>();
        outgoingEdges = new HashMap<Integer, List<Integer>>();
        nodeCaptions = new HashMap<Integer, String>();
        status = new HashMap<Integer, Integer>();
        collapsed = new HashSet<Integer>();
        
        edgeLabels = new HashMap<Integer, String>();
        startNodes = new HashMap<Integer, Integer>();
        endNodes = new HashMap<Integer, Integer>();
    }
    
    @Override
    public void addEdge(int edgeID, int start, int end, String label)
    {
        if (roots.contains(end)) roots.remove(end);
        edgeLabels.put(edgeID, label);
        startNodes.put(edgeID, start);
        endNodes.put(edgeID,end);
        getOutgoingEdges(start).add(edgeID);
        getIncomingEdges(end).add(edgeID);
        nextEdgeID = Math.max(edgeID + 1, nextEdgeID);
    }

    @Override
    public int addEdge(int start, int end, String label)
    {
        if (roots.contains(end)) roots.remove(end);
        int edgeID = getNextFreeEdgeID();
        edgeLabels.put(edgeID, label);
        startNodes.put(edgeID, start);
        endNodes.put(edgeID, end);
        getOutgoingEdges(start).add(edgeID);
        getIncomingEdges(end).add(edgeID);
        return edgeID;
    }
    

    @Override
    public void addNode(int id, String caption, int nodeStatus)
    {
        roots.add(id);
        setNodeCaption(id, caption);
        setNodeStatus(id, nodeStatus);
        nextNodeID = Math.max(id + 1, nextNodeID);
    }

    @Override
    public int addNode(String caption, int nodeStatus)
    {
        int nodeID = getNextFreeNodeID();
        roots.add(nodeID);
        nodeCaptions.put(nodeID, caption);
        status.put(nodeID,nodeStatus);
        return nodeID;
    }

    @Override
    public void collapse(int nodeID)
    {
        if (nodeID != -1)
        {
            collapsed.add(nodeID);
        }
    }
    
    @Override
    public void decollapse(int nodeID)
    {
        collapsed.remove(nodeID);
    }
    
    @Override
    public void decollapseAll()
    {
        collapsed = new HashSet<Integer>();
    }

    @Override
    public String getEdgeLabel(int edgeID)
    {
        String label = edgeLabels.get(edgeID);
        if (label == null)
        {
            return "";
        }
        else
        {
            return label;
        }
    }

    @Override
    public List<Integer> getIncomingEdges(int nodeID)
    {
        List<Integer> incoming = incomingEdges.get(nodeID);
        if (incoming == null)
        {
            incoming = new ArrayList<Integer>();
            incomingEdges.put(nodeID, incoming);
        }
        return incoming;
    }
    
    @Override
	public List<Integer> getVisibleParents(int nodeID)
    {
    	ArrayList<Integer> ancestors = new ArrayList<Integer>();
        // System.err.println("\t Actual children for node " + nodeID + ": " +
        // treeModel.getChildren(nodeID,treeLayer));
        List<Integer> incomingEdges = getIncomingEdges(nodeID);
        for (int incoming : incomingEdges)
        {
            ancestors.add(getStartNode(incoming));
        }
        for (int i = 0; i < ancestors.size(); i++)
        {
            boolean nodeIsVisible = nodeIsVisible(ancestors.get(i));
            if (!nodeIsVisible)
            {
                List<Integer> edges = getIncomingEdges(ancestors.remove(i));
                for (int edge : edges)
                {
                    ancestors.add(getStartNode(edge));
                }
                i--;
            }
        }
        return ancestors;
    }

    @Override
    public String getNodeCaption(int nodeID)
    {
        String caption = nodeCaptions.get(nodeID);
        if (caption == null)
        {
            return "";
        }
        else
        {
            return caption;
        }
    }

    @Override
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

    @Override
    public List<Integer> getOutgoingEdges(int nodeID)
    {
        List<Integer> outgoing = outgoingEdges.get(nodeID);
        if (outgoing == null)
        {
            outgoing = new ArrayList<Integer>();
            outgoingEdges.put(nodeID, outgoing);
        }
        return outgoing;
    }
    
    @Override
	public ArrayList<Integer> getVisibleChildren(int nodeID)
    {
        ArrayList<Integer> descendants = new ArrayList<Integer>();
        // System.err.println("\t Actual children for node " + nodeID + ": " +
        // treeModel.getChildren(nodeID,treeLayer));
        if (isCollapsed(nodeID)) return descendants;
        List<Integer> outgoingEdges = getOutgoingEdges(nodeID);
        for (int outgoing : outgoingEdges)
        {
            descendants.add(getEndNode(outgoing));
        }
        for (int i = 0; i < descendants.size(); i++)
        {
            boolean nodeIsVisible = nodeIsVisible(descendants.get(i));
            if (!nodeIsVisible)
            {
                List<Integer> edges = getOutgoingEdges(descendants.remove(i));
                for (int edge : edges)
                {
                    descendants.add(getEndNode(edge));
                }
                i--;
            }
        }
        return descendants;
    }
    
    public boolean nodeIsVisible(int nodeID)
    {
        // TODO: implement special DAG collapsing behavior
        //if (model.hasCollapsedAncestor(nodeID)) return false;
        return true;
    }

    @Override
    public int getSize()
    {
        return nodeCaptions.size();
    }

    @Override
    public boolean isCollapsed(int nodeID)
    {
        return collapsed.contains(nodeID);
    }

    @Override
    public void setEdgeLabel(int edgeID, String label)
    {
        edgeLabels.put(edgeID, label);
    }

    @Override
    public void setNodeCaption(int nodeID, String caption)
    {
        nodeCaptions.put(nodeID, caption);      
    }

    @Override
    public void setNodeStatus(int nodeID, int newStatus)
    {
        status.put(nodeID, newStatus);      
    }
    
    protected int getNextFreeNodeID()
    {
        int nextIDHyp = nextNodeID;
        while (nodeCaptions.get(nextIDHyp) != null)
        {
            nextIDHyp++;
        }
        nextNodeID = nextIDHyp + 1;
        return nextIDHyp;
    }
    
    protected int getNextFreeEdgeID()
    {
        int nextIDHyp = nextEdgeID;
        while (startNodes.get(nextIDHyp) != null)
        {
            nextIDHyp++;
        }
        nextEdgeID = nextIDHyp + 1;
        return nextIDHyp;
    }
    
    public static KahinaDAG importXML(Document dom)
    {
        KahinaMemDAG m = new KahinaMemDAG();
        Element treeElement = dom.getDocumentElement();
        NodeList childNodes = treeElement.getChildNodes();
        for (int i = 0; i < childNodes.getLength(); i++)
        {
            Node n = childNodes.item(i);
            if (n.getNodeName().equals("node"))
            {
                importNode(m, (Element) n);
            }
            else if (n.getNodeName().equals("edge"))
            {
                importEdge(m, (Element) n);
            }
        }
        return m;
    }

    private static void importNode(KahinaMemDAG m, Element node)
    {
        int nodeID = 0;
        if (node.getAttribute("id").length() > 0)
        {
            nodeID = Integer.parseInt(node.getAttribute("id"));
        } 
        else
        {
            nodeID = m.getNextFreeNodeID();
        }
        m.roots.add(nodeID);
        m.nodeCaptions.put(nodeID, node.getAttribute("caption"));
        if (node.getAttribute("status").length() > 0)
        {
            m.status.put(nodeID, Integer.parseInt(node.getAttribute("status")));
        }
    }
    
    private static void importEdge(KahinaMemDAG m, Element edge)
    {
        int edgeID = 0;
        if (edge.getAttribute("id").length() > 0)
        {
            edgeID = Integer.parseInt(edge.getAttribute("id"));
        } 
        else
        {
            edgeID = m.getNextFreeEdgeID();
        }
        String label = edge.getAttribute("label");
        int start = Integer.parseInt(edge.getAttribute("start"));
        int end = Integer.parseInt(edge.getAttribute("end"));
        m.addEdge(edgeID, start, end, label);
    }

    @Override
    public int getEndNode(int edgeID)
    {
        Integer endNode = endNodes.get(edgeID);
        if (endNode == null)
        {
            return -1;
        }
        else
        {
            return endNode;
        }
    }

    @Override
    public int getStartNode(int edgeID)
    {
        Integer startNode = startNodes.get(edgeID);
        if (startNode == null)
        {
            return -1;
        }
        else
        {
            return startNode;
        }
    }

    @Override
    public void setEndNode(int edgeID, int endNode)
    {
        endNodes.put(edgeID, endNode);   
    }

    @Override
    public void setStartNode(int edgeID, int startNode)
    {
        startNodes.put(edgeID, startNode);         
    }

    @Override
    public Iterable<Integer> getEdgeIDIterator()
    {
        return startNodes.keySet();
    }

    @Override
    public Iterable<Integer> getNodeIDIterator()
    {
        return nodeCaptions.keySet();
    }

    @Override
    public Set<Integer> getRoots()
    {
        return roots;
    }

    @Override
    //TODO: use a decent shortest path algorithm!
    public List<Integer> findShortestPathFromRoot(int nodeID)
    {
        List<Integer> path = new LinkedList<Integer>();
        int minLength = Integer.MAX_VALUE;
        for (int edge : getIncomingEdges(nodeID))
        {
            List<Integer> subpath = findShortestPathFromRoot(getStartNode(edge));
            if (subpath.size() < minLength)
            {
                minLength = subpath.size();
                path = subpath;
            }
        }
        path.add(nodeID);
        return path;
    }
}
