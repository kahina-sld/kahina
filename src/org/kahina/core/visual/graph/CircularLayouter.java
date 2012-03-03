package org.kahina.core.visual.graph;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class CircularLayouter extends KahinaGraphLayouter
{ 
    //internal array structure (empty := -1)
    int[] array;
    Map<Integer,Integer> vertexToIndex;
    
    public boolean FLAG_USE_INVISIBLE_EDGES = true;

    @Override
    public void computeInitialLayout()
    {
        System.err.print("Computing initial graph layout ...");
        Set<Integer> vertices = view.getModel().getVertices();
        int numVertices = vertices.size();
        //fill the grid with vertices as they come
        array = new int[numVertices];
        vertexToIndex = new HashMap<Integer,Integer>();
        int i = 0;
        for (int vertex : vertices)
        {
            array[i] = vertex;
            vertexToIndex.put(vertex, i);
            i++;
        }
        //compute the coordinates corresponding to the array
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
        // TODO Auto-generated method stub
        
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
        int zoomLevel = view.getConfig().getZoomLevel();
        int midpointX = zoomLevel * 50 + 10;
        int midpointY = zoomLevel * 50 + 10;
        double degreeIncrement = Math.PI * 2 / array.length;
        double degree = 0.0;
        for (int i = 0; i < array.length; i++)
        {
            int x = (int) (midpointX - 50 * zoomLevel * Math.sin(degree));
            int y = (int) (midpointY - 50 * zoomLevel * Math.cos(degree));
            xCoord.put(array[i],x);
            yCoord.put(array[i],y);
            degree += degreeIncrement;
        }
    }

}
