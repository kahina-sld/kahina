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
    
    public boolean FLAG_USE_INVISIBLE_EDGES = true;

    @Override
    public void computeInitialLayout()
    {
        System.err.print("Computing initial graph layout ...");
        Set<Integer> vertices = view.getModel().getVertices();
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
        System.err.print("Global optimization of a grid graph layout ...");
        for (int i = 0; i < grid.length; i++)
        {
            for (int j = 0; j < grid[i].length; j++)
            {
               System.err.println(" Optimizing vertex " + grid[i][j]);
               optimizePositionOfVertexAt(i,j);
            }
        }
        System.err.println(" done in " + (System.currentTimeMillis() - startTime) + " ms.");
    }
    
    @Override
    public void optimizeVtxPosAllEdges(int v)
    {
        System.err.println(" Optimizing vertex " + v);
        optimizePositionOfVertexAt(gridX.get(v),gridY.get(v));
    }

    @Override
    public void optimizeVtxPosVisibleEdges(int v)
    {
        FLAG_USE_INVISIBLE_EDGES = false;
        System.err.println(" Optimizing vertex " + v);
        optimizePositionOfVertexAt(gridX.get(v),gridY.get(v));
        FLAG_USE_INVISIBLE_EDGES = true;
    }
    
    private void optimizePositionOfVertexAt(int i, int j)
    {
        int node = grid[i][j];
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
                int bestImprovement = Integer.MIN_VALUE;
                int bestX = -1;
                int bestY = -1;
                int candImprovement = 0;
                int candX = (int) Math.floor(optimalX);
                int candY = (int) Math.floor(optimalY);
                for (int k = candX; k <= candX + 1 && k < grid.length; k++)
                {
                    for (int l = candY; l <= candY + 1 && l < grid[k].length; l++)
                    {
                        candImprovement = computeMoveOrSwapImprovement(node,k,l);
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
                    int centerX = bestX;
                    int centerY = bestY;
                    int x = centerX;
                    int y = centerY;
                    int nBorder = y - 1;
                    if (nBorder < 0) nBorder = 0;
                    int wBorder = x - 1;
                    if (wBorder < 0) wBorder = 0;
                    int eBorder = x + 1;
                    if (eBorder > grid.length) wBorder = grid.length;
                    int sBorder = y + 1;
                    if (sBorder > grid[0].length) sBorder = grid[0].length;
                    int maxLoops = 4;
                    //spiral out until some improving position is found or maxLoops is reached
                    for (int loop = 0; loop < maxLoops; loop++)
                    {
                        // outward to the north until northern border is reached (normally just one step)
                        y--;
                        while (y > nBorder)
                        {
                            if (y >= 0)
                            {
                                candImprovement = computeMoveOrSwapImprovement(node,x,y);
                                if (candImprovement > bestImprovement)
                                {
                                    bestX = x;
                                    bestY = y;
                                    bestImprovement = candImprovement;
                                }
                            }
                            else
                            {
                                break;
                            }
                            y--;
                        }
                        // along the northern border to the east until eastern border is reached
                        if (y < 0)
                        {
                            //just jump to the eastern border
                            x = eBorder;
                            y = 0;
                        }
                        else
                        {
                            while (x < eBorder && x < grid.length)
                            {
                                candImprovement = computeMoveOrSwapImprovement(node,x,y);
                                if (candImprovement > bestImprovement)
                                {
                                    bestX = x;
                                    bestY = y;
                                    bestImprovement = candImprovement;
                                }
                                x++;                            
                            }
                        }
                        // along the eastern border until southern border is reached
                        if (x >= grid.length)
                        {
                            //just jump to the southern border
                            y = sBorder;
                            x = grid.length - 1;
                        }
                        else
                        {
                            while (y < sBorder && y < grid[x].length)
                            {
                                candImprovement = computeMoveOrSwapImprovement(node,x,y);
                                if (candImprovement > bestImprovement)
                                {
                                    bestX = x;
                                    bestY = y;
                                    bestImprovement = candImprovement;
                                }
                                y++;                     
                            }
                        }
                        // along the southern border until western border is reached
                        if (y >= grid[x].length)
                        {
                            //just jump to the western border
                            x = wBorder;
                            y = grid[x].length - 1;
                        }
                        else
                        {
                            while (x > wBorder && x >= 0)
                            {
                                candImprovement = computeMoveOrSwapImprovement(node,x,y);
                                if (candImprovement > bestImprovement)
                                {
                                    bestX = x;
                                    bestY = y;
                                    bestImprovement = candImprovement;
                                }
                                x--;  
                            }
                        }
                        // along the western border until northern border is reached
                        if (x < 0)
                        {
                            //just jump to the western border
                            y = nBorder;
                            x = 0;
                        }
                        else
                        {
                            while (y > nBorder && y >= 0)
                            {
                                candImprovement = computeMoveOrSwapImprovement(node,x,y);
                                if (candImprovement > bestImprovement)
                                {
                                    bestX = x;
                                    bestY = y;
                                    bestImprovement = candImprovement;
                                }
                                y--;                      
                            }
                        }
                        // along the northern border back to the center
                        if (y < 0)
                        {
                            //just jump back to the center
                            x = centerX;
                            y = 0;
                        }
                        else
                        {
                            while (x < centerX)
                            {
                                candImprovement = computeMoveOrSwapImprovement(node,x,y);
                                if (candImprovement > bestImprovement)
                                {
                                    bestX = x;
                                    bestY = y;
                                    bestImprovement = candImprovement;
                                }
                                x++;           
                            }
                        }
                        //spiral ring complete, extend outward
                        nBorder--;
                        wBorder--;
                        eBorder++;
                        sBorder++;
                        //swap with the optimum of last loop if improvement
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
                            break;
                        }
                    }
                }
            }
        }
    }
    
    public void refreshCoordinates()
    {
        int offset = view.getConfig().getZoomLevel() * 2;
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
    
    private int computeMoveOrSwapImprovement(int v, int x, int y)
    {
        int result = 0;
        //System.err.print("Compare vertex " + v + " at (" + gridX.get(v) + "," + gridY.get(v) + ") to (" + x + "," + y + ")");
        int v2 = grid[x][y];
        if (v2 == -1)
        {
            result = computeImprovement(v, gridX.get(v), gridY.get(v), x, y);
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
        for (int neighbor : view.getModel().getNeighbors(node))
        {
            if (FLAG_USE_INVISIBLE_EDGES || view.isVertexVisible(neighbor))
            {
                distanceSum += sumDistance(x,y,gridX.get(neighbor), gridY.get(neighbor));
            }
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
        return (grid.length + 1) * view.getConfig().getZoomLevel() * 2;
    }

    @Override
    public int getDisplayWidth()
    {
        if (grid.length == 0) return 0;
        return (grid[0].length + 1) * view.getConfig().getZoomLevel() * 2;
    }
}
