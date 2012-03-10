package org.kahina.core.visual.graph;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class CircularLayouter extends KahinaGraphLayouter
{ 
    //internal array structure (empty := -1)
    int[] array;
    Map<Integer,Integer> vertexToIndex;
    
    public boolean FLAG_USE_INVISIBLE_EDGES = true;
    
    public boolean VERBOSE = false;

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
        long startTime = System.currentTimeMillis();
        System.err.print("Global optimization of a circular graph layout ...");
        for (int i = 0; i < array.length; i++)
        {
           optimizePositionOfVertexAt(i);
        }
        //compute the coordinates corresponding to the grid
        refreshCoordinates();
        System.err.println(" done in " + (System.currentTimeMillis() - startTime) + " ms.");
        
    }

    @Override
    public void optimizeVtxPosAllEdges(int v)
    {
        optimizePositionOfVertexAt(vertexToIndex.get(v));
        //compute the coordinates corresponding to the grid
        refreshCoordinates();     
    }

    @Override
    public void optimizeVtxPosVisibleEdges(int v)
    {
        FLAG_USE_INVISIBLE_EDGES = false;
        optimizePositionOfVertexAt(vertexToIndex.get(v));
        FLAG_USE_INVISIBLE_EDGES = true;
        //compute the coordinates corresponding to the grid
        refreshCoordinates();     
    }
    
    private void optimizePositionOfVertexAt(int i)
    {
        if (VERBOSE) System.err.print(" Optimizing vertex " + array[i] + "[" + i + "] : |");
        int node = array[i];
        if (node != -1)
        {
            List<Integer> neighbors = null;
            if (FLAG_USE_INVISIBLE_EDGES)
            {
                neighbors = view.getModel().getNeighbors(node);
            }
            else
            {
                neighbors = view.getVisibleNeighbors(node);
            }
            if (neighbors.size() > 0)
            {
                //determine index in the array which would be the ideal point for the node
                int clockwiseMovement = 0;
                for (int neighbor : neighbors)
                {
                    if (VERBOSE) System.err.print(vertexToIndex.get(neighbor) + "|");
                    clockwiseMovement += minimumClockwiseDistance(node,neighbor);
                }
                clockwiseMovement /= neighbors.size();
                int optimalIndex = mod(i - clockwiseMovement,array.length);
                if (VERBOSE) System.err.println(" -> " + optimalIndex);
                //compute for a range of indices around the optimal index
                //   how much swapping the node with the node there improves the layout
                int maxDistance = 8;
                int bestImprovement = Integer.MIN_VALUE;
                int best = -1;
                int candImprovement = 0;
                int cand = optimalIndex;
                int size = array.length;
                for (int j = cand - maxDistance; j < cand + maxDistance + 1; j++)
                {
                    candImprovement = computeMoveOrSwapImprovement(node, mod(j,size));
                    //System.err.println("  " + mod(j,size) + " -> candImprovement " + candImprovement);
                    if (candImprovement > bestImprovement)
                    {
                        best = mod(j,size);
                        bestImprovement = candImprovement;
                    }
                }
                //if the best swap yields improvement > 0: swap
                if (bestImprovement > 0)
                {
                    int swapVert = array[best];
                    if (swapVert == -1)
                    {
                        array[i] = -1;
                        array[best] = node;
                        vertexToIndex.put(node, best);
                    }
                    else
                    {
                        swapNodes(node,swapVert);
                    }
                }
            }
            else
            {
                if (VERBOSE)
                {
                    System.err.println("(none)|");
                }
            }
        }
    }
    
    private int minimumClockwiseDistance(int v1, int v2)
    {
        int clockwise = mod((vertexToIndex.get(v1) - vertexToIndex.get(v2)),array.length);    
        int counterClockwise = mod((vertexToIndex.get(v2) - vertexToIndex.get(v1)),array.length); 
        if (counterClockwise > clockwise)
        {
            clockwise = - counterClockwise;
        }
        return clockwise;
    }
    
    private void swapNodes(int v1, int v2)
    {
        if (VERBOSE) System.err.println("  swapNodes(" + v1 + "," + v2 + ")");
        int idx1 = vertexToIndex.get(v1);
        int idx2 = vertexToIndex.get(v2);
        array[idx1] = v2;
        array[idx2] = v1;
        vertexToIndex.put(v2, idx1);
        vertexToIndex.put(v1, idx2);
    }
    
    private int computeMoveOrSwapImprovement(int v, int idx)
    {
        int result = 0;
        //System.err.print("Compare vertex " + v + " at (" + gridX.get(v) + "," + gridY.get(v) + ") to (" + x + "," + y + ")");
        int v2 = array[idx];
        if (v2 == -1)
        {
            result = computeImprovement(v, vertexToIndex.get(v), idx);
        }
        else
        {
            result = computeSwapImprovement(v, v2);
        }
        //System.err.println(", Improvement: " + result);
        return result;
    }
    
    private int computeSwapImprovement(int v1, int v2)
    {
        int idx1 = vertexToIndex.get(v1);
        int idx2 = vertexToIndex.get(v2);
        return computeImprovement(v1,idx1,idx2) + computeImprovement(v2,idx2,idx1);
    }
    
    //the negative of the distance sum change if vertex is moved from index idx1 to idx2
    private int computeImprovement(int node, int idx1, int idx2)
    {
        return computeDistanceSum(node, idx1) - computeDistanceSum(node, idx2);
    }
    
    //evaluate the sum of the outgoing edge lengths if node were at index idx
    private int computeDistanceSum(int node, int idx)
    {
        //System.err.println("computeDistanceSum(" + node + "," + idx + ") = ");
        int distanceSum = 0;
        for (int neighbor : view.getModel().getNeighbors(node))
        {
            if (FLAG_USE_INVISIBLE_EDGES || view.isVertexVisible(neighbor))
            {
                distanceSum += minDistance(idx, vertexToIndex.get(neighbor));
            }
        }
        //System.err.println(distanceSum + ";");
        return distanceSum;
    }
    
    //minimum distance between indices idx1 and idx2
    private int minDistance(int idx1, int idx2)
    {
        //System.err.println("minDist(" + idx1 + "," + idx2 + ")");
        //System.err.println("  " + "mod(" + (idx2 - idx1) + "," + array.length + ") = " + mod((idx2 - idx1),array.length));
        //System.err.println("  " + "mod(" + (idx1 - idx2) + "," + array.length + ") = " + mod((idx1 - idx2),array.length));
        return Math.min(mod((idx2 - idx1),array.length), mod((idx1 - idx2), array.length));
    }
    
    //real modulo operation (also for negative integers)
    private int mod(int a, int b)
    {
        if (a >= 0)
        {
            return a % b;
        }
        return (a-((a / b - 1)*b)) % b;
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
            if (view.getConfig().getSpecialVertexPositionPolicy() == KahinaGraphViewOptions.SPECIAL_VERTICES_SEPARATE && view.isVertexSpecial(array[i]))
            {
                int x = (int) (midpointX - 25 * zoomLevel * Math.sin(degree));
                int y = (int) (midpointY - 25 * zoomLevel * Math.cos(degree));
                xCoord.put(array[i],x);
                yCoord.put(array[i],y);
            }
            else
            {
                int x = (int) (midpointX - 50 * zoomLevel * Math.sin(degree));
                int y = (int) (midpointY - 50 * zoomLevel * Math.cos(degree));
                xCoord.put(array[i],x);
                yCoord.put(array[i],y);
            }
            degree += degreeIncrement;
        }
    }

}
