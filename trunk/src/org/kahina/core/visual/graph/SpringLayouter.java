package org.kahina.core.visual.graph;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class SpringLayouter extends KahinaGraphLayouter
{
    //internal positions on grid (expressed in [0,1] x [0,1])
    double[] xPos;
    double[] yPos;
    
    Map<Integer,Integer>  vertexToIndex;
    
    public boolean FLAG_USE_INVISIBLE_EDGES = true;

    @Override
    public void computeInitialLayout()
    {
        System.err.print("Computing random initial graph layout ...");
        Set<Integer> vertices = view.getModel().getVertices();
        int numVertices = vertices.size();
        xPos = new double[numVertices];
        yPos = new double[numVertices]; 
        vertexToIndex = new HashMap<Integer,Integer>();
        int id = 0;
        for (int vertex : vertices)
        {
            vertexToIndex.put(vertex, id);
            xPos[id] = Math.random();
            yPos[id] = Math.random();
            id++;
        }
        //compute the coordinates corresponding to the internal positions
        refreshCoordinates();
        System.err.println(" done.");   
    }

    @Override
    public int getDisplayHeight()
    {
        return view.getConfig().getZoomLevel() * 100 + 20;
    }

    @Override
    public int getDisplayWidth()
    {
        return view.getConfig().getZoomLevel() * 100 + 20;
    }

    @Override
    public void optimize()
    {
        double REJECTION_FORCE = 0.001 / xPos.length;
        double SPRING_STRENGTH = 0.05;
        double SPRING_LENGTH = 0.5 / Math.sqrt(xPos.length);
        double[] xForces = new double[xPos.length];
        double[] yForces = new double[yPos.length];
        for (int vertex1 : vertexToIndex.keySet())
        {
            int id1 = vertexToIndex.get(vertex1);
            //repulsion between vertices
            for (int vertex2 : vertexToIndex.keySet())
            {
                if (vertex1 != vertex2)
                {
                    double[] vector = unitVector(vertex2,vertex1);
                    double dist = euclideanDistance(vertex2,vertex1);
                    xForces[id1] += REJECTION_FORCE / Math.pow(dist, 2) * vector[0];
                    yForces[id1] += REJECTION_FORCE / Math.pow(dist, 2) * vector[1]; 
                    //System.err.println("repulse(" + vertex1 + "," + vertex2 + ") = ("
                    //                  + REJECTION_FORCE / Math.pow(dist, 2) * vector[0] + ","
                    //                  + REJECTION_FORCE / Math.pow(dist, 2) * vector[1] + ")"); 
                }
            }
            //edges as springs keeping vertices together
            //System.err.println("Neighbors of vertex " + vertex1 + ": " + this.view.getModel().getNeighbors(vertex1));
            for (int vertex2 : this.view.getModel().getNeighbors(vertex1))
            {
                double[] vector = unitVector(vertex1,vertex2);
                double dist = euclideanDistance(vertex2,vertex1);
                double factor = SPRING_STRENGTH * (dist - SPRING_LENGTH);
                xForces[id1] += factor * vector[0];
                yForces[id1] += factor * vector[1]; 
                //System.err.println("attract(" + vertex1 + "," + vertex2 + ") = ("
                //        + factor * vector[0] + "," + factor * vector[1] + ")"); 
            }
        }

        for (int vertex : vertexToIndex.keySet())
        {
            int id = vertexToIndex.get(vertex);
            //a hard constraint prevents vertices from exceeding the view boundaries
            xPos[id] += xForces[id];
            if (xPos[id] < 0.1) xPos[id] = 0.1/(-xPos[id]+1);
            if (xPos[id] > 0.9) xPos[id] = 0.9 + 0.1/(xPos[id]-1);
            yPos[id] += yForces[id];
            if (yPos[id] < 0.1) yPos[id] = 0.1/(-yPos[id]+1);
            if (yPos[id] > 0.9) yPos[id] = 0.9 + 0.1/(yPos[id]-1);
            //System.err.println("xPos[" + id + "] += " + xForces[id] + " = " + xPos[id]);
        }
        refreshCoordinates();
    }

    @Override
    public void optimizeVtxPosAllEdges(int v)
    {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void optimizeVtxPosVisibleEdges(int v)
    {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void refreshCoordinates()
    {
        for (int vertex : vertexToIndex.keySet())
        {
            int id = vertexToIndex.get(vertex);
            xCoord.put(vertex, (int) (xPos[id] * getDisplayWidth() - 20) + 10);
            yCoord.put(vertex, (int) (yPos[id] * getDisplayHeight() - 20) + 10);
        }
    }
    
    private double[] unitVector(int vertex1, int vertex2)
    {
        double dist = euclideanDistance(vertex1, vertex2);
        double[] unitVector = new double[2];
        unitVector[0] = (xPos[vertexToIndex.get(vertex2)] - xPos[vertexToIndex.get(vertex1)]) / dist;
        unitVector[1] = (yPos[vertexToIndex.get(vertex2)] - yPos[vertexToIndex.get(vertex1)]) / dist;
        return unitVector;
    }
    
    private double euclideanDistance(int vertex1, int vertex2)
    {
        double x1 = xPos[vertexToIndex.get(vertex1)];
        double x2 = xPos[vertexToIndex.get(vertex2)];
        double y1 = yPos[vertexToIndex.get(vertex1)];
        double y2 = yPos[vertexToIndex.get(vertex2)];
        return Math.sqrt(Math.pow(x2-x1,2) + Math.pow(y2-y1,2));
    }
}
