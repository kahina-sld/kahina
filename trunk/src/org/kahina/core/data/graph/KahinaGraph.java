package org.kahina.core.data.graph;

import org.kahina.core.data.KahinaObject;

public abstract class KahinaGraph extends KahinaObject
{
    public abstract void addVertex(int v);
    public abstract void addVertex(int v, int status);
    public abstract void addVertex(int v, String label);
    public abstract void addVertex(int v, String label, int status);
    
    public abstract void addDirectedEdge(int v1, int v2);
    public abstract void addDirectedEdge(int v1, int v2, int status);
    public abstract void addDirectedEdge(int v1, int v2, String label);
    public abstract void addDirectedEdge(int v1, int v2, String label, int status);
    
    public abstract void addUndirectedEdge(int v1, int v2);
    public abstract void addUndirectedEdge(int v1, int v2, int status);
    public abstract void addUndirectedEdge(int v1, int v2, String label);
    public abstract void addUndirectedEdge(int v1, int v2, String label, int status);
    
    public abstract void setEdgeLabel(int v1, int v2, String label);
    public abstract String getEdgeLabel(int v1, int v2);
    
    public abstract void setEdgeStatus(int v1, int v2, int status);
    public abstract int getEdgeStatus(int v1, int v2);
}
