package org.kahina.core.visual.graph;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class SpringLayouter extends KahinaGraphLayouter
{
    //internal positions on grid (expressed in [0,100] x [0,100])
    double[] xPos;
    double[] yPos;
    
    double xScaling = 1.0;
    double xTranslation = 0;
    double yScaling = 1.0;
    double yTranslation = 0;
    
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
            xPos[id] = Math.random() * 100;
            yPos[id] = Math.random() * 100;
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
        double REJECTION_FORCE = 1;
        double SPRING_STRENGTH = 1;
        double SPRING_LENGTH = 50 / Math.sqrt(xPos.length);
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
                //double factor = SPRING_STRENGTH * (dist - SPRING_LENGTH);
                double factor = SPRING_STRENGTH * Math.log(dist / SPRING_LENGTH);
                xForces[id1] += factor * vector[0];
                yForces[id1] += factor * vector[1]; 
                //System.err.println("attract(" + vertex1 + "," + vertex2 + ") = ("
                //        + factor * vector[0] + "," + factor * vector[1] + ")"); 
            }
        }
        
        for (int vertex : vertexToIndex.keySet())
        {
            int id = vertexToIndex.get(vertex);
            xPos[id] += xForces[id];
            yPos[id] += yForces[id];
        }
        
        /*for (int vertex : vertexToIndex.keySet())
        {
            int id = vertexToIndex.get(vertex);
            //TODO: debug this (sometimes coordinates do exceed the bounds)
            //a hard constraint prevents vertices from exceeding the view boundaries
            xPos[id] += xForces[id];
            if (xPos[id] < 10) xPos[id] = 10/(-xPos[id]+100);
            if (xPos[id] > 90) xPos[id] = 90 + 10/(xPos[id]-100);
            yPos[id] += yForces[id];
            if (yPos[id] < 10) yPos[id] = 10/(-yPos[id]+100);
            if (yPos[id] > 90) yPos[id] = 90 + 10/(yPos[id]-100);
            //System.err.println("xPos[" + id + "] += " + xForces[id] + " = " + xPos[id]);
        }*/
        refreshCoordinates();
    }
    
    private void optimizePositionOfVertex(int vertex)
    {
        double REJECTION_FORCE = 1;
        double SPRING_STRENGTH = 1;
        double SPRING_LENGTH = 10;
        //TODO: adapt this to the number of visible nodes
        //double SPRING_LENGTH = 50 / Math.sqrt(xPos.length);

        int id1 = vertexToIndex.get(vertex);
        //repulsion between vertices
        for (int vertex2 : vertexToIndex.keySet())
        {
            if (vertex != vertex2 && view.isVertexVisible(vertex2))
            {
                double[] vector = unitVector(vertex2,vertex);
                double dist = euclideanDistance(vertex2,vertex);
                xPos[id1] += REJECTION_FORCE / Math.pow(dist, 2) * vector[0];
                yPos[id1] += REJECTION_FORCE / Math.pow(dist, 2) * vector[1]; 
                //System.err.println("repulse(" + vertex1 + "," + vertex2 + ") = ("
                //                  + REJECTION_FORCE / Math.pow(dist, 2) * vector[0] + ","
                //                  + REJECTION_FORCE / Math.pow(dist, 2) * vector[1] + ")"); 
            }
        }
        //edges as springs keeping vertices together
        //System.err.println("Neighbors of vertex " + vertex1 + ": " + this.view.getModel().getNeighbors(vertex1));
        for (int vertex2 : this.view.getModel().getNeighbors(vertex))
        {
            if (FLAG_USE_INVISIBLE_EDGES || view.isEdgeVisible(vertex, vertex2))
            {
                double[] vector = unitVector(vertex,vertex2);
                double dist = euclideanDistance(vertex2,vertex);
                //double factor = SPRING_STRENGTH * (dist - SPRING_LENGTH);
                double factor = SPRING_STRENGTH * Math.log(dist / SPRING_LENGTH);
                xPos[id1] += factor * vector[0];
                yPos[id1] += factor * vector[1]; 
                //System.err.println("attract(" + vertex1 + "," + vertex2 + ") = ("
                //        + factor * vector[0] + "," + factor * vector[1] + ")"); 
            }
        }
    }

    @Override
    public void optimizeVtxPosAllEdges(int v)
    {
        optimizePositionOfVertex(v); 
    }

    @Override
    public void optimizeVtxPosVisibleEdges(int v)
    {
        FLAG_USE_INVISIBLE_EDGES = false;
        System.err.println(" Optimizing vertex " + v);
        optimizePositionOfVertex(v);
        FLAG_USE_INVISIBLE_EDGES = true;    
    }

    @Override
    public void refreshCoordinates()
    {
        adaptScaling();
        int zoomLevel = view.getConfig().getZoomLevel();
        for (int vertex : vertexToIndex.keySet())
        {
            int id = vertexToIndex.get(vertex);
            xCoord.put(vertex, (int) ((xPos[id] * xScaling + xTranslation) * zoomLevel) + 10);
            yCoord.put(vertex, (int) ((yPos[id] * yScaling + yTranslation) * zoomLevel) + 10);
        }
    }
    
    private void adaptScaling()
    {
        //scale the resulting coordinates to fit the picture
        double maxX = 50.1;
        double minX = 49.9;
        double maxY = 50.1;
        double minY = 49.9;
        for (int vertex : vertexToIndex.keySet())
        {
            if (view.isVertexVisible(vertex))
            {
                int id = vertexToIndex.get(vertex);
                if (xPos[id] > maxX) maxX = xPos[id];
                else if (xPos[id] < minX) minX = xPos[id];
                if (yPos[id] > maxY) maxY = yPos[id];
                else if (yPos[id] < minY) minY = yPos[id];
            }
        }
        //System.err.println("maxX: " + maxX + " minX: " + minX + " maxY: " + maxY + " minY: " + minY);
        xScaling = 100 / (maxX - minX);
        xTranslation = -((maxX + minX) / 2 * xScaling - 50);
        yScaling = 100 / (maxY - minY);
        yTranslation = -((maxY + minY) / 2 * yScaling - 50);
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
