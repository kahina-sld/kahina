package org.kahina.core.data.graph;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;
import java.util.Set;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.visual.graph.KahinaGraphView;

public abstract class KahinaGraph extends KahinaObject
{
    /**
     * Creates an empty graph, an AdjacListsGraph by default.
     * @return an empty KahinaGraph (implemented as an AdjacListsGraph by default)
     */
    public static KahinaGraph createEmptyGraph()
    {
        return new AdjacListsGraph();
    }
    
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
    
    /**
     * Imports a graph from a file in Trivial Graph Format (TGF).
     * @param fileName path to the input file in TGF format
     * @return a KahinaGraph object representing the graph from the file
     */
    public static KahinaGraph importTGF(String fileName)
    {
        KahinaGraph graph = createEmptyGraph();
        try
        {
            BufferedReader in = new BufferedReader(new FileReader(fileName));
            int phase = 0; //0 -> read vertex labels, 1 -> read edges and their labels
            while (in.ready())
            {
                String line = in.readLine();
                if (line.equals("#"))
                {
                    phase = 1;
                }
                else if (phase == 0)
                {
                    int firstSpace = line.indexOf(' ');
                    try
                    {
                        if (firstSpace != -1)
                        {
                            String vertexID = line.substring(0, firstSpace);
                            String vertexLabel = line.substring(firstSpace + 1);
                            graph.addVertex(Integer.parseInt(vertexID), vertexLabel);
                        }
                        else
                        {
                            //assume empty label
                            graph.addVertex(Integer.parseInt(line));
                        }  
                    }
                    catch (NumberFormatException e)
                    {
                        System.err.println("ERROR during TGF input: Vertex line not starting with ID!");
                        return graph;
                    }
                }
                else if (phase == 1)
                {
                    int firstSpace = line.indexOf(' ');
                    try
                    {
                        if (firstSpace != -1)
                        {
                            String v1ID = line.substring(0, firstSpace);
                            String rest = line.substring(firstSpace + 1);
                            int secondSpace = rest.indexOf(' ');
                            if (secondSpace != -1)
                            {
                                String v2ID = rest.substring(0, secondSpace);
                                String edgeLabel = rest.substring(secondSpace + 1);
                                graph.addDirectedEdge(Integer.parseInt(v1ID), 
                                                      Integer.parseInt(v2ID), edgeLabel);
                            }
                            else
                            {
                                //assume empty label
                                graph.addDirectedEdge(Integer.parseInt(v1ID), Integer.parseInt(rest));
                            }
                        }
                        else
                        {
                            System.err.println("ERROR during TGF input: Malformed edge line!");
                            return graph;
                        }  
                    }
                    catch (NumberFormatException e)
                    {
                        System.err.println("ERROR during TGF input: Edge line not starting with ID!");
                        return graph;
                    }
                }
            }
            in.close();
        }
        catch (FileNotFoundException e)
        {
            System.err.println("ERROR: Graph file not found. Returning empty graph!");
        }
        catch (IOException e)
        {
            System.err.println("ERROR: IOException while reading TGF file. Returning empty graph!");
        }
        return graph;
    }
    
    public void exportTGF(String fileName) throws IOException
    {
        try
        {
            BufferedWriter out = new BufferedWriter(new FileWriter(fileName));
            //print vertex IDs and vertex labels
            for (int vertex : getVertices())
            {
                out.write(vertex + " " + getVertexLabel(vertex) + "\n");
            }
            out.write("#\n");
            //print all edges (undirected edges in both directions) with their labels
            for (int vertex1 : getVertices())
            {
                for (int vertex2 : getNeighbors(vertex1))
                {
                    out.write(vertex1 + " " + vertex2 + " " + getEdgeLabel(vertex1,vertex2) + "\n");
                }
            }
            out.close();
        }
        catch (FileNotFoundException e)
        {
            System.err.println("ERROR: File for TGF output not found. Aborting TGF output!");
        }
    }
}
