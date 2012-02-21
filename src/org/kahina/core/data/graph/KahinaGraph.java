package org.kahina.core.data.graph;

import java.util.List;
import java.util.Set;

import org.kahina.core.data.KahinaObject;

public abstract class KahinaGraph extends KahinaObject
{
    public abstract Set<Integer> getVertices();
    
    public abstract void addVertex(int v);
    public abstract void addVertex(int v, int status);
    public abstract void addVertex(int v, String label);
    public abstract void addVertex(int v, String label, int status);
    
    public abstract List<Integer> getNeighbors(int v);
    
    public abstract void addDirectedEdge(int v1, int v2);
    public abstract void addDirectedEdge(int v1, int v2, int status);
    public abstract void addDirectedEdge(int v1, int v2, String label);
    public abstract void addDirectedEdge(int v1, int v2, String label, int status);
    
    public abstract void addUndirectedEdge(int v1, int v2);
    public abstract void addUndirectedEdge(int v1, int v2, int status);
    public abstract void addUndirectedEdge(int v1, int v2, String label);
    public abstract void addUndirectedEdge(int v1, int v2, String label, int status);
    
    public abstract void setVertexLabel(int v, String label);
    public abstract String getVertexLabel(int v);
    
    public abstract void setEdgeLabel(int v1, int v2, String label);
    public abstract String getEdgeLabel(int v1, int v2);
    
    public abstract void setVertexStatus(int v, int status);
    public abstract int getVertexStatus(int v);
    
    public abstract void setEdgeStatus(int v1, int v2, int status);
    public abstract int getEdgeStatus(int v1, int v2);
}
