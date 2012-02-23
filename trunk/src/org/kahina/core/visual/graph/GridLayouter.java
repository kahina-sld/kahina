package org.kahina.core.visual.graph;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class GridLayouter extends KahinaGraphLayouter
{
    // determine height/width ratio of the grid
    double gridRatio = 1.0;
    //internal grid structure (empty := -1)
    int[][] grid;
    Map<Integer,Integer> gridX;
    Map<Integer,Integer> gridY;

    @Override
    public void computeInitialLayout()
    {
        System.err.print("Computing initial graph layout ...");
        Set<Integer> vertices = g.getVertices();
        int numVertices = vertices.size();
        //determine correct dimensions for grid (y = rx => x*rx > numVertices)
        double optimalX = Math.sqrt(numVertices / gridRatio);
        int xDim = (int) Math.ceil(optimalX);
        int yDim = 0;
        if (xDim != 0) //cover the case where the graph has zero vertices
        {
            yDim = (int) Math.ceil((numVertices + 0.0) / xDim);
        }
        //fill the grid with vertices as they come
        grid = new int[xDim][yDim];
        gridX = new HashMap<Integer,Integer>();
        gridY = new HashMap<Integer,Integer>();  
        int i = 0, j = 0;
        for (int vertex : vertices)
        {
            grid[i][j] = vertex;
            gridX.put(vertex, i);
            gridY.put(vertex, j);
            i++;
            if (i >= xDim)
            {
                i = 0;
                j++;
            }
        }
        //fill the rest of the grid with -1
        while (j < yDim)
        {
            grid[i][j] = -1;
            i++;
            if (i >= xDim)
            {
                i = 0;
                j++;
            }
        }
        //compute the coordinates corresponding to the grid
        refreshCoordinates();
        System.err.println(" done.");
    }

    @Override
    public void optimize()
    {
        long startTime = System.currentTimeMillis();
        System.err.print("Optimizing a grid graph layout ...");
        for (int i = 0; i < grid.length; i++)
        {
            for (int j = 0; j < grid[i].length; j++)
            {
                int node = grid[i][j];
                if (node != -1)
                {
                    List<Integer> neighbors = g.getNeighbors(node);
                    if (neighbors.size() > 1)
                    {
                        //determine center of neighboring vertices as ideal point for node
                        double vectorX = 0.0;
                        double vectorY = 0.0;
                        for (int neighbor : neighbors)
                        {
                            vectorX += gridX.get(neighbor) - i;
                            vectorY += gridY.get(neighbor) - j;
                        }
                        vectorX /= neighbors.size();
                        vectorY /= neighbors.size();
                        double optimalX = i + vectorX;
                        double optimalY = j + vectorY;
                        //compute for any of the four grid points around optimal point
                        //   how much swapping the node with the node there improves the layout
                        int bestImprovement = 0;
                        int bestX = -1;
                        int bestY = -1;
                        int candImprovement = 0;
                        int candX = (int) Math.floor(optimalX);
                        int candY = (int) Math.floor(optimalY);
                        for (int k = candX; k <= candX + 1; k++)
                        {
                            for (int l = candY; l <= candY + 1; l++)
                            {
                                int candVert = grid[k][l];
                                if (candVert == -1)
                                {
                                    candImprovement = computeImprovement(node, i, j, k, l);
                                }
                                else
                                {
                                    candImprovement = computeSwapImprovement(node, candVert);
                                }
                                if (candImprovement > bestImprovement)
                                {
                                    bestX = k;
                                    bestY = l;
                                    bestImprovement = candImprovement;
                                }
                            }
                        }
                        //if the best swap yields improvement > 0: swap
                        if (bestImprovement > 0)
                        {
                            int swapVert = grid[bestX][bestY];
                            if (swapVert == -1)
                            {
                                grid[i][j] = -1;
                                grid[bestX][bestY] = node;
                                gridX.put(node, bestX);
                                gridY.put(node, bestY);
                            }
                            else
                            {
                                swapNodes(node,swapVert);
                            }
                        }
                        //otherwise still try to move the node as much as possible towards its optimal position
                        else
                        {
                            //start from vertex with best improvement
                            int x = bestX;
                            int y = bestY;
                            int nBorder = y - 1;
                            int wBorder = x - 1;
                            int eBorder = x + 1;
                            int sBorder = y + 1;
                            int maxSteps = 100;
                            int steps = 0;
                            //spiral out until some improving position is found or maxSteps is reached
                            while (steps < maxSteps)
                            {
                                // outward to the north until northern border is reached
                                while (y > nBorder)
                                {
                                    y--;
                                    
                                }
                                // along the northern border to the east until eastern border is reached
                                while (x < eBorder)
                                {
                                    x++;
                                    
                                }
                                // along the eastern border until southern border is reached
                                while (y < sBorder)
                                {
                                    y++;
                                    
                                }
                                // along the southern border until western border is reached
                                while (x > wBorder)
                                {
                                    x--;
                                    
                                }
                                // along the western border until northern border is reached
                                while (y > nBorder)
                                {
                                    y--;
                                    
                                }
                                // along the northern border back to the center
                                while (x < bestX)
                                {
                                    x++;
                                    
                                }
                                //spiral ring complete, extend outward
                                nBorder--;
                                wBorder--;
                                eBorder++;
                                sBorder++;
                                //TODO: special treatment of grid boundaries
                            }
                        }
                    }
                    else if (neighbors.size() == 1)
                    {
                        //TODO: try out swaps with the eight vertices currently next to the single neighbor
                        //int neighbor = neighbors.get(0);
                        //int centerX = gridX.get(neighbor);
                        //int centerY = gridY.get(neighbor);
                    }
                }
            }
        }
        //compute the coordinates corresponding to the grid
        refreshCoordinates();
        System.err.println(" done in " + (System.currentTimeMillis() - startTime) + " ms.");
    }
    
    public void refreshCoordinates()
    {
        int offset = config.getZoomLevel() * 2;
        int currentX = offset;
        for (int i = 0; i < grid.length; i++)
        {
            int currentY = offset;
            for (int j = 0; j < grid[i].length; j++)
            {
                int node = grid[i][j];
                if (node != -1)
                {
                    xCoord.put(node, currentX);
                    yCoord.put(node, currentY);
                }
                currentY += offset;
            }
            currentX += offset;
        }
    }
    
    private void swapNodes(int v1, int v2)
    {
        int x1 = gridX.get(v1);
        int y1 = gridY.get(v1);
        int x2 = gridX.get(v2);
        int y2 = gridY.get(v2);
        grid[x1][y1] = v2;
        grid[x2][y2] = v1;
        gridX.put(v2, x1);
        gridX.put(v1, x2);
        gridY.put(v2, y1);
        gridY.put(v1, y2);
    }
    
    private int computeSwapImprovement(int v1, int v2)
    {
        int x1 = gridX.get(v1);
        int y1 = gridY.get(v1);
        int x2 = gridX.get(v2);
        int y2 = gridY.get(v2);
        return computeImprovement(v1,x1,y1,x2,y2) + computeImprovement(v2,x2,y2,x1,y1);
    }
    
    //evaluate the distance sum change if node is moved from (x1,y1) to (x2,y2)
    private int computeImprovement(int node, int x1, int y1, int x2, int y2)
    {
        return computeDistanceSum(node, x1, y1) - computeDistanceSum(node, x2, y2);
    }
    
    //evaluate the sum of the outgoing edge lengths if node were at (x,y)
    private int computeDistanceSum(int node, int x, int y)
    {
        int distanceSum = 0;
        for (int neighbor : g.getNeighbors(node))
        {
            distanceSum += sumDistance(x,y,gridX.get(neighbor), gridY.get(neighbor));
        }
        return distanceSum;
    }
    
    //non-Euclidean distance measure between points (x1,y1) and (x2,y2)
    private int sumDistance(int x1, int y1, int x2, int y2)
    {
        return Math.abs(y2 - y1) + Math.abs(x2 - x1);
    }

    @Override
    public int getDisplayHeight()
    {
        return grid.length * config.getZoomLevel() * 2;
    }

    @Override
    public int getDisplayWidth()
    {
        if (grid.length == 0) return 0;
        return grid[0].length * config.getZoomLevel() * 2;
    }
}
