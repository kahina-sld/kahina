package org.kahina.core.data.graph;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class AdjacListsGraph extends KahinaGraph
{
    //adjacency list representation for graph structures
    //also: the central vertex registry; vertices without neighbors must be mentioned here!
    //directed graphs can be expressed by only storing adjacents in one direction
    protected Map<Integer, List<Integer>> adjacents; 
    protected Map<Integer, String> vertexLabels; //labels can be displayed on the vertices
    protected Map<Integer, Integer> vertexStatus;
    protected Map<Integer, Map<Integer, String>> edgeLabels; //labels can be displayed on the edges
    protected Map<Integer, Map<Integer, Integer>> edgeStatus;
    
    //store the ID of the next node that is going to be added
    private int nextID = 0;
    
    public AdjacListsGraph()
    {        
        super();
        
        adjacents = new HashMap<Integer, List<Integer>>();
        vertexLabels = new HashMap<Integer, String>();
        vertexStatus = new HashMap<Integer, Integer>();
        edgeLabels = new HashMap<Integer, Map<Integer, String>>();
        edgeStatus  = new HashMap<Integer, Map<Integer, Integer>>();
    }
    
    protected int getNextFreeID()
    {
        int nextIDHyp = nextID;
        while (adjacents.get(nextIDHyp) != null)
        {
            nextIDHyp++;
        }
        nextID = nextIDHyp + 1;
        return nextIDHyp;
    }
    
    public Set<Integer> getVertices()
    {
        return adjacents.keySet();
    }
    
    public void addVertex(int v)
    {
        if (adjacents.get(v) != null)
        {
            adjacents.put(v, new LinkedList<Integer>());
        }
        else
        {
            System.err.println("Warning: vertex " + v + " was already defined!");
        }
    }
    
    public void addVertex(int v, int status)
    {
        addVertex(v);
        vertexStatus.put(v,status);
    }
    
    public void addVertex(int v, String label)
    {
        addVertex(v);
        vertexLabels.put(v,label);
    }
    
    public void addVertex(int v, String label, int status)
    {
        addVertex(v);
        vertexLabels.put(v,label);
        vertexStatus.put(v,status);
    }
    
    public List<Integer> getNeighbors(int v)
    {
        List<Integer> neighbors = adjacents.get(v);
        if (neighbors == null)
        {
            neighbors = new LinkedList<Integer>();
            System.err.println("Vertex " + v + " not defined. Returning empty neighbor list.");
        }
        return neighbors;
    }
    
    public void addDirectedEdge(int v1, int v2)
    {
        List<Integer> adjacentList = adjacents.get(v1);
        if (adjacentList != null)
        {
            if (adjacentList.contains(v2))
            {
                System.err.println("Edge (" + v1 + "," + v2 + ") was already in graph!");
            }
            else
            {
                adjacentList.add(v2);
            }
        }
        else
        {
            System.err.println("WARNING: vertex " + v1 + " not defined, edge (" + v1 + "," + v2 + ") was NOT added!");
        }
    }
    
    public void addDirectedEdge(int v1, int v2, int status)
    {
        addDirectedEdge(v1,v2);
        setEdgeStatus(v1,v2,status);
    }
    
    public void addDirectedEdge(int v1, int v2, String label)
    {
        addDirectedEdge(v1,v2);
        setEdgeLabel(v1,v2,label);
    }
    
    public void addDirectedEdge(int v1, int v2, String label, int status)
    {
        addDirectedEdge(v1,v2);
        setEdgeLabel(v1,v2,label);
        setEdgeStatus(v1,v2,status);
    }
    
    public void addUndirectedEdge(int v1, int v2)
    {
        addDirectedEdge(v1,v2);
        addDirectedEdge(v2,v1);
    }
    
    public void addUndirectedEdge(int v1, int v2, int status)
    {
        addUndirectedEdge(v1,v2);
        setEdgeStatus(v1,v2,status);
        setEdgeStatus(v2,v1,status);
    }
    
    public void addUndirectedEdge(int v1, int v2, String label)
    {
        addUndirectedEdge(v1,v2);
        setEdgeLabel(v1,v2,label);
        setEdgeLabel(v2,v1,label);
    }
    
    public void addUndirectedEdge(int v1, int v2, String label, int status)
    {
        addUndirectedEdge(v1,v2);
        setEdgeStatus(v1,v2,status);
        setEdgeStatus(v2,v1,status);
        setEdgeLabel(v1,v2,label);
        setEdgeLabel(v2,v1,label);
    }
    
    public void setEdgeLabel(int v1, int v2, String label)
    {
        Map<Integer,String> submap = edgeLabels.get(v1);
        if (submap == null)
        {
            submap = new HashMap<Integer,String>();
            edgeLabels.put(v1, submap);
        }
        submap.put(v2, label);
    }
    
    public String getEdgeLabel(int v1, int v2)
    {
        Map<Integer,String> submap = edgeLabels.get(v1);
        if (submap == null)
        {
            return "";
        }
        else
        {
            String label = submap.get(v2);
            if (label == null) return "";
            return label;
        }
    }
    
    public void setEdgeStatus(int v1, int v2, int status)
    {
        Map<Integer,Integer> submap = edgeStatus.get(v1);
        if (submap == null)
        {
            submap = new HashMap<Integer,Integer>();
            edgeStatus.put(v1, submap);
        }
        submap.put(v2, status);
    }
    
    /**
     * Returns the status ID of the edge between two nodes.
     * @param v1 the ID of the edge's start node
     * @param v2 the ID of the edge's end node
     * @return the status ID of (v1,v2); 0 if edge or label undefined
     */
    public int getEdgeStatus(int v1, int v2)
    {
        Map<Integer,Integer> submap = edgeStatus.get(v1);
        if (submap == null)
        {
            return 0;
        }
        else
        {
            Integer status = submap.get(v2);
            if (status == null) return 0;
            return 0;
        }
    }
}
