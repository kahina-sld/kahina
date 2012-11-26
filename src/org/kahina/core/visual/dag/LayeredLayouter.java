package org.kahina.core.visual.dag;

import java.awt.BasicStroke;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Stroke;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.kahina.core.visual.tree.WidthVector;

public class LayeredLayouter extends KahinaDAGLayouter
{
    public static boolean VERBOSE = false;
    
    // layered structure for drawing; also used for reverse indexing
    ArrayList<List<Integer>> nodeLevels;
    Set<Integer> allNodes;
    
    private Map<Integer, Integer> nodeHeights;
    private Map<Integer, Integer> nodeWidths;
    
    //private variables for internal calculations across functions
    private int totalTreeHeight;
    private int totalTreeWidth;
    //private Map<Integer, WidthVector> subtreeWidths;
    private Map<Integer, Integer> drawingParents;
    //private int maxNodeWidth;
    
    // hack to allow precalculations from outside any drawing method
    private Graphics2D g;
    
    public void newDAG(KahinaDAGView view)
    {
        this.view = view;
        xCoord = view.getXCoordinates();
        yCoord = view.getYCoordinates();
        
        nodeWidths = view.getNodeWidths();
        
        computeLayout();
    }

    @Override
    public void computeLayout()
    {
        nodeLevels = new ArrayList<List<Integer>>();
        allNodes = new HashSet<Integer>();
        
        nodeHeights = new HashMap<Integer, Integer>();
        
        //subtreeWidths = new HashMap<Integer, WidthVector>();
        drawingParents = new HashMap<Integer, Integer>();
        
        if (view != null) refreshCoordinates();
    }
    
    private void createNodeLayers()
    {
        if (VERBOSE) System.err.println("BEGIN: Create node layers");
        
        nodeLevels.clear();
        allNodes.clear();
        
        //do a specialized variant of topological sort on nodes
        HashMap<Integer, Integer> levelForNode = new HashMap<Integer, Integer>();
        LinkedList<Integer> nodeLevelAgenda = new LinkedList<Integer>();
        for (int rootID : view.getModel().getRoots())
        {
            levelForNode.put(rootID, 0);
            nodeLevelAgenda.add(rootID);
            allNodes.add(rootID);
        }
        while (nodeLevelAgenda.size() > 0)
        {
            int nodeID = nodeLevelAgenda.remove(0);
            List<Integer> parents = view.getModel().getVisibleParents(nodeID);
            int maxParentLayer = -1;
            int maxParent = -1;
            for (int parent : parents)
            {
                Integer parentLayer = levelForNode.get(parent);
                if (parentLayer == null)
                {
                    maxParentLayer = -2;
                    maxParent = -1;
                    break;
                }
                else
                {
                    if (parentLayer > maxParentLayer)
                    {
                        maxParentLayer = parentLayer;
                        maxParent = parent;
                    }
                }
            }
            if (maxParentLayer != -2)
            {
                //if all the parent layers were known, assign highest layer + 1 to node
                levelForNode.put(nodeID, maxParentLayer + 1);
                drawingParents.put(nodeID, maxParent);
                List<Integer> children = view.getModel().getVisibleChildren(nodeID);
                //since a node can be child of several nodes, check whether it is on the agenda already
                for (int child : children)
                {
                    if (!allNodes.contains(child))
                    {
                        nodeLevelAgenda.add(child);
                        allNodes.add(child);                    
                    }
                }
            }
            else
            {
                //otherwise wait until all parent layers were computed
                nodeLevelAgenda.add(nodeID);
            }
        }
        //move roots down if deep root position policy is enforced
        if (view.config.getRootPositionPolicy() == KahinaDAGViewOptions.ROOT_POSITION_DEEP)
        {
            for (int rootID : view.getModel().getRoots())
            {
                int newRootLevel = view.getModel().getSize();
                for (int edgeID : view.getModel().getOutgoingEdges(rootID))
                {
                    int childLevel = levelForNode.get(view.getModel().getEndNode(edgeID));
                    if (childLevel <= newRootLevel) newRootLevel = childLevel - 1;
                }
                levelForNode.put(rootID, newRootLevel);
            }
        }

        //assemble the layers from the node -> layer map
        int maxLevel = -1;
        for (int nodeID : levelForNode.keySet())
        {
            int level = levelForNode.get(nodeID);
            if (level > maxLevel)
            {
                //make space for the additional layers
                maxLevel = level;
                while (nodeLevels.size() <= maxLevel)
                {
                    nodeLevels.add(new ArrayList<Integer>());
                }
            }
            nodeLevels.get(level).add(nodeID);
        }
        
        if (VERBOSE) System.err.println("COMPLETE: Create node levels");
        if (VERBOSE) System.err.println("Levels:\n" + showLevels());
        nodeLevels = BarycenterCrossingReduction.minimizeCrossings(view.getModel(), nodeLevels);
        if (VERBOSE) System.err.println("COMPLETE: Cross reduction on node levels");
        if (VERBOSE) System.err.println("Levels:\n" + showLevels());
    }
    

    public int getDisplayWidth()
    {
        return totalTreeWidth;
    }

    public int getDisplayHeight()
    {
        return totalTreeHeight;
    }

    @Override
    public void refreshCoordinates()
    {
        if (VERBOSE) System.err.println("BEGIN: Calculate coordinates");

        FontMetrics fm = getFontMetrics(new Font(Font.SANS_SERIF, Font.PLAIN, view.getConfig().getNodeSize()), new BasicStroke(1), view.getConfig().getNodeSize());

        createNodeLayers();
        
        xCoord.clear();
        yCoord.clear();

        int zoomLevel = view.getConfig().getZoomLevel();
        int horiDistance = view.getConfig().getHorizontalDistance();
        int vertDistance = view.getConfig().getVerticalDistance();
        int nodeSize = view.getConfig().getNodeSize();
        int numLayers = nodeLevels.size();
        if (numLayers > 0)
        {
            int maxLayer = determineMaximumLayer();
            int maxLayerSize = nodeLevels.get(maxLayer).size();

            totalTreeWidth = (maxLayerSize + 2) * horiDistance * zoomLevel;
            totalTreeHeight = (numLayers + 2) * vertDistance * zoomLevel + 10;
            
            //initial placement on all layers: centered, at regular intervals
            for (int layerID = 0; layerID < numLayers; layerID++)
            {
                List<Integer> layer = nodeLevels.get(layerID);
                int offset = (int) (horiDistance * zoomLevel * (1 + (maxLayerSize - layer.size()) / 2.0));
                for (int node : layer)
                {
                    Dimension nodeDimension = computeNodeDimension(fm, node);
                    nodeWidths.put(node, nodeDimension.width);
                    nodeHeights.put(node, nodeDimension.height);
                    
                    xCoord.put(node, offset);
                    yCoord.put(node, layerID * vertDistance * zoomLevel + 10);
                    offset += horiDistance * zoomLevel;
                }
                
            }
            
            //move downward, rearrange nodes according to positions of already determined ancestors
            for (int layerID = maxLayer + 1; layerID < numLayers; layerID++)
            {
                List<Integer> layer = nodeLevels.get(layerID);
                //move all nodes as far left as they want (but never beyond their left neighbor)
                for (int i = 0; i < layer.size(); i++)
                {
                    int node = layer.get(i);
                    Dimension nodeDimension = computeNodeDimension(fm, node);
                    nodeWidths.put(node, nodeDimension.width);
                    nodeHeights.put(node, nodeDimension.height);
                    
                    int optimalX = computeAncestorOptimalXPos(node);
                    
                    if (optimalX < xCoord.get(node))
                    {
                        if (i == 0)
                        {
                            xCoord.put(node, optimalX);
                        }
                        else
                        {
                            int neighborX = xCoord.get(layer.get(i-1));
                            if (neighborX > optimalX - horiDistance * zoomLevel)
                            {
                                xCoord.put(node, neighborX + horiDistance * zoomLevel);
                            }
                        }
                    }
                }
                //move all nodes as far right as they want (but never beyond their right neighbor)
                for (int i = layer.size() - 1; i >= 0; i--)
                {
                    int node = layer.get(i);
                    Dimension nodeDimension = computeNodeDimension(fm, node);
                    nodeWidths.put(node, nodeDimension.width);
                    nodeHeights.put(node, nodeDimension.height);
                    
                    int optimalX = computeAncestorOptimalXPos(node);
                    
                    if (optimalX > xCoord.get(node))
                    {
                        if (i == layer.size() - 1)
                        {
                            xCoord.put(node, optimalX);
                        }
                        else
                        {
                            int neighborX = xCoord.get(layer.get(i+1));
                            if (neighborX < optimalX + horiDistance * zoomLevel)
                            {
                                xCoord.put(node, neighborX - horiDistance * zoomLevel);
                            }
                        }
                    }
                }
            }     
            
            //move upward, rearrange nodes according to positions of already determined descendants
            for (int layerID = maxLayer - 1; layerID >= 0; layerID--)
            {
                List<Integer> layer = nodeLevels.get(layerID);
                //move all nodes as far left as they want (but never beyond their left neighbor)
                for (int i = 0; i < layer.size(); i++)
                {
                    int node = layer.get(i);
                    Dimension nodeDimension = computeNodeDimension(fm, node);
                    nodeWidths.put(node, nodeDimension.width);
                    nodeHeights.put(node, nodeDimension.height);
                    
                    int optimalX = computeDescendantOptimalXPos(node);
                    
                    if (optimalX < xCoord.get(node))
                    {
                        if (i == 0)
                        {
                            xCoord.put(node, optimalX);
                        }
                        else
                        {
                            int neighborX = xCoord.get(layer.get(i-1));
                            if (neighborX > optimalX - horiDistance * zoomLevel)
                            {
                                xCoord.put(node, neighborX + horiDistance * zoomLevel);
                            }
                        }
                    }
                }
                //move all nodes as far right as they want (but never beyond their right neighbor)
                for (int i = layer.size() - 1; i >= 0; i--)
                {
                    int node = layer.get(i);
                    Dimension nodeDimension = computeNodeDimension(fm, node);
                    nodeWidths.put(node, nodeDimension.width);
                    nodeHeights.put(node, nodeDimension.height);
                    
                    int optimalX = computeDescendantOptimalXPos(node);
                    
                    if (optimalX > xCoord.get(node))
                    {
                        if (i == layer.size() - 1)
                        {
                            xCoord.put(node, optimalX);
                        }
                        else
                        {
                            int neighborX = xCoord.get(layer.get(i+1));
                            if (neighborX < optimalX + horiDistance * zoomLevel)
                            {
                                xCoord.put(node, neighborX - horiDistance * zoomLevel);
                            }
                        }
                    }
                }
            }

        }
        
    }
    
    public String showLevels()
    {
        String levelString = "";
        for (List<Integer> nodeLevel : nodeLevels)
        {
            levelString += nodeLevel + "\n";
        }
        return levelString;
    }
    
    private int determineMaximumLayer()
    {
        int maxLayer = 0;
        int maxLayerSize = 0;
        for (int i = 0; i < nodeLevels.size(); i++)
        {
            int size = nodeLevels.get(i).size();
            if (size > maxLayerSize)
            {
               maxLayerSize = size;
               maxLayer = i;
            }
        }
        return maxLayer;
    }
    
    private int computeAncestorOptimalXPos(int nodeID)
    {
        int averageX = 0; 
        int numComputedParents = 0;
        for (int edge : view.getModel().getIncomingEdges(nodeID))
        {
            int parent = view.getModel().getStartNode(edge);
            Integer parentX = xCoord.get(parent);
            if (parentX != null)
            {
                averageX += parentX;
                numComputedParents++;
            }
        }
        if (numComputedParents > 0)
        {
            averageX /= numComputedParents;
        }
        else
        {
            averageX = totalTreeWidth / 2;
        }
        return averageX;
    }
    
    private int computeDescendantOptimalXPos(int nodeID)
    {
        int averageX = 0; 
        int numComputedChildren = 0;
        for (int edge : view.getModel().getOutgoingEdges(nodeID))
        {
            int child = view.getModel().getEndNode(edge);
            Integer childX = xCoord.get(child);
            if (childX != null)
            {
                averageX += childX;
                numComputedChildren++;
            }
        }
        if (numComputedChildren > 0)
        {
            averageX /= numComputedChildren;
        }
        else
        {
            averageX = totalTreeWidth / 2;
        }
        return averageX;
    }

    //TODO: make this work also with bottom-up display orientation
    public int getNodeAtCoordinates(int x, int y)
    {
        // binary search over node levels to determine row
        int lowerIndex = 0;
        int upperIndex = nodeLevels.size() - 1;
        int middleIndex = (lowerIndex + upperIndex) / 2;
        int middleBound = yCoord.get(nodeLevels.get(middleIndex).get(0)) + view.getConfig().getZoomLevel();
        if (upperIndex != 0) // simply take the only existing level if there is only one
        {
            while (lowerIndex + 1 < upperIndex)
            {
                // System.err.println("lower: " + lowerIndex + " upper: " +
                // upperIndex + " middle bound: " + middleBound + " y: " + y);
                if (middleBound >= y)
                {
                    upperIndex = middleIndex;
                } 
                else
                {
                    lowerIndex = middleIndex;
                }
                middleIndex = (lowerIndex + upperIndex) / 2;
                middleBound = yCoord.get(nodeLevels.get(middleIndex).get(0)) + view.getConfig().getZoomLevel();
            }
            if (y < middleBound) upperIndex--;
        }
        // System.err.println("Node Level: " + upperIndex);

        // binary search over nodes in
        List<Integer> selectedLevel = nodeLevels.get(upperIndex);
        int candidateNode = -1;
        if (selectedLevel.size() == 1)
        {
            candidateNode = selectedLevel.get(0);
        } 
        else
        {
            lowerIndex = 0;
            upperIndex = selectedLevel.size() - 1;
            middleIndex = (lowerIndex + upperIndex) / 2;
            FontMetrics fm = getFontMetrics(new Font(Font.SANS_SERIF, Font.PLAIN, view.getConfig().getNodeSize()), new BasicStroke(1), view.getConfig().getNodeSize());
            int width = fm.stringWidth(view.getModel().getNodeCaption(selectedLevel.get(middleIndex)));
            middleBound = xCoord.get(selectedLevel.get(middleIndex)) + width / 2 + 2;
            // System.err.println("lower: " + lowerIndex + " upper: " +
            // upperIndex + " middle bound: " + middleBound + " x: " + x);
            while (lowerIndex + 1 < upperIndex)
            {
                if (middleBound >= x)
                {
                    upperIndex = middleIndex;
                } 
                else
                {
                    lowerIndex = middleIndex;
                }
                middleIndex = (lowerIndex + upperIndex) / 2;
                width = fm.stringWidth(view.getModel().getNodeCaption(selectedLevel.get(middleIndex)));
                middleBound = xCoord.get(selectedLevel.get(middleIndex)) + width / 2 - 2;
                // System.err.println("lower: " + lowerIndex + " upper: " +
                // upperIndex + " middle bound: " + middleBound + " x: " + x);
            }
            if (x < middleBound) upperIndex--;
            candidateNode = selectedLevel.get(upperIndex);
        }
        // System.err.println("Potentially clicked node: " + candidateNode);

        // test coordinates against exact boundaries of candidate node
        int xLeft = xCoord.get(candidateNode);
        int xRight = xLeft + view.getNodeWidth(candidateNode);
        int yTop = yCoord.get(candidateNode);
        int yBottom = yTop + view.getConfig().getNodeSize();
        // System.err.println("test: " + xLeft + " <= " + x + " <= " + xRight +
        // "; " + yTop + " <= " + y + " <= " + yBottom);
        if (xLeft <= x && x <= xRight && yTop <= y && y <= yBottom)
        {
            // System.err.println("Click on node: " + candidateNode);
        } 
        else
        {
            // System.err.println("No node found!");
            return -1;
        }
        return candidateNode;
    }
    
    private int getRelevantAntecedent(int nodeID)
    {
        List<Integer> antecedents = view.getModel().getVisibleParents(nodeID);
        if (antecedents.size() == 0) return -1;
        if (antecedents.size() == 1) return antecedents.get(0);
        //get the leftmost direct antecedent
        else
        {
            int minAntID = antecedents.get(0);
            int minX = xCoord.get(minAntID);
            for (int i = 1; i < antecedents.size(); i++)
            {
                int antID = antecedents.get(i);
                int antX = xCoord.get(antID);
                if (antX < minX)
                {
                    minAntID = antID;
                    minX = antX;
                }
            }
            return minAntID;
        }
        //int middleIndex = antecedents.size() / 2;
        //return antecedents.get(middleIndex);
    }
    
    public FontMetrics getFontMetrics(Font f, Stroke s, int fontSize)
    {
        // hack to allow precalculations from outside any drawing method
        if (g == null)
        {
            BufferedImage bufferedImage = new BufferedImage(2, 2, BufferedImage.TYPE_4BYTE_ABGR_PRE);
            g = bufferedImage.createGraphics();
        }
        g.setFont(new Font(f.getFontName(), f.getStyle(), fontSize));
        g.setStroke(s);
        return g.getFontMetrics();
    }
    
    private Dimension computeNodeDimension(FontMetrics fm, int nodeID)
    {
        int maxWidth = 0;
        String tag = view.getModel().getNodeCaption(nodeID);
        String[] stringParts = tag.split("\\\\n");
        for (String part : stringParts)
        {
            int width = fm.stringWidth(part);
            if (width > maxWidth) maxWidth = width;
        }        
        return new Dimension(maxWidth, stringParts.length * view.config.getNodeSize() + 4);
    }
}
