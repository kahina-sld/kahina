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
    private Map<Integer, WidthVector> subtreeWidths;
    private Map<Integer, Integer> drawingParents;
    private int maxNodeWidth;
    
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
        
        subtreeWidths = new HashMap<Integer, WidthVector>();
        drawingParents = new HashMap<Integer, Integer>();
        
        maxNodeWidth = -1;
        
        refreshCoordinates();
    }
    
    private void createNodeLayers()
    {
        if (VERBOSE) System.err.println("BEGIN: Create node layers");
        
        //do a specialized variant of topological sort on nodes
        HashMap<Integer, Integer> levelForNode = new HashMap<Integer, Integer>();
        int rootID = view.getModel().getRootID();
        levelForNode.put(rootID, 0);
        LinkedList<Integer> nodeLevelAgenda = new LinkedList<Integer>();
        nodeLevelAgenda.add(rootID);
        allNodes.add(rootID);
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
    
    private WidthVector constructWidthVector(int node)
    {
        if (VERBOSE) System.err.println("--------------------------------------------");
        if (VERBOSE) System.err.println("Width vector computation for node " + node);
        List<Integer> children = view.getModel().getVisibleChildren(node);
        if (VERBOSE) System.err.println(" Candidate children: " + children);
        for (int i = 0; i < children.size(); i++)
        {
            if (VERBOSE) System.err.println(" Child: " + children.get(i) + " Drawing Parent: " + drawingParents.get(children.get(i)));
            if (drawingParents.get(children.get(i)) != node)
            {
                children.remove(i);
                i--;
            }
        }
        int nodeWidth = nodeWidths.get(node) + view.getConfig().getHorizontalDistance() * view.getConfig().getZoomLevel();
        WidthVector sum = new WidthVector(nodeWidth / 2, nodeWidth / 2);
        if (children.size() > 0)
        {
            sum = subtreeWidths.get(children.get(0)).copy();
            for (int i = 1; i < children.size(); i++)
            {
                sum = WidthVector.adjoin(sum, subtreeWidths.get(children.get(i)));
            }
            sum.start.add(0, nodeWidth / 2);
            sum.end.add(0, nodeWidth / 2);
        }
        if (VERBOSE) System.err.println("Width vector for node " + node + ": " + sum.toString());
        if (VERBOSE) System.err.println("--------------------------------------------");
        return sum;
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
        // System.err.println(showLevels());
        // System.err.println(terminalLayer);

        totalTreeWidth = 50;
        totalTreeHeight = (nodeLevels.size() + 2) * view.getConfig().getVerticalDistance() * view.getConfig().getNodeSize() + 10;
        
        int zoomLevel = view.getConfig().getZoomLevel();

        if (view.getModel().getRootID() != -1)
        {
            if (VERBOSE) System.err.println("BEGIN: Calculate subtree widths");
            // calculate (maximum) subtree width for each node bottom-up
            for (int i = nodeLevels.size() - 1; i >= 0; i--)
            {
                if (VERBOSE) System.err.println("Node level: " + i);
                for (int node : nodeLevels.get(i))
                {
                    Dimension nodeDimension = computeNodeDimension(fm, node);
                    nodeWidths.put(node, nodeDimension.width);
                    nodeHeights.put(node, nodeDimension.height);
                    // System.err.println("labelWidth(" +
                    // getContentfulTreeModel().getNodeCaption(node) + ") = " +
                    // nodeLabelWidth);
                    if (maxNodeWidth < nodeDimension.width) maxNodeWidth = nodeDimension.width;               
                    subtreeWidths.put(node, constructWidthVector(node));
                    //if (verbose)  System.err.println("  Node:" + node + " VisChildren:" + children + " WidthVector:" + subtreeWidths.get(node));
                }
            }
            if (VERBOSE) System.err.println("COMPLETE: Calculate subtree widths");
            if (VERBOSE) System.err.println("maxNodeWidth = " + maxNodeWidth);
            xCoord.put(view.getModel().getRootID(), subtreeWidths.get(view.getModel().getRootID()).maximumLeftDistance() + view.getConfig().getHorizontalDistance() * zoomLevel);
            for (int i = 0; i < nodeLevels.size(); i++)
            {
                if (VERBOSE) System.err.println("Node level: " + i);
                List<Integer> nodes = nodeLevels.get(i);
                int xOffset = 0;
                if (nodes.size() > 0) xOffset = subtreeWidths.get(nodes.get(0)).maximumLeftDistance() + view.getConfig().getHorizontalDistance() * zoomLevel;
                int parent = -1;
                WidthVector subtreeWidth = new WidthVector();
                WidthVector lastSubtreeWidth;
                for (int node : nodes)
                {
                    if (VERBOSE) System.err.print("  Node:" + node);
                    lastSubtreeWidth = subtreeWidth;
                    subtreeWidth = subtreeWidths.get(node);
                    xOffset += WidthVector.computeNecessaryDistance(lastSubtreeWidth, subtreeWidth); //+ horizontalDistance * fontSize;
                    // switch to children of next parent node --> jump in x offset
                    int newParent = drawingParents.get(node);
                    if (VERBOSE) System.err.print(" VisParent:" + newParent);
                    if (i > 0 && newParent != parent)
                    {
                        parent = newParent;
                        if (VERBOSE) System.err.print(" SubtreeWidths:" + subtreeWidths.get(parent));
                        // old variant of xOffset computation
                        // xOffset = (int)((nodeX.get(parent) - (subtreeWidths.get(parent).getStart(1) * 0.5 - 0.5) * horizontalDistance * fontSize));
                        xOffset = xCoord.get(parent)  + nodeWidths.get(parent) / 2 - subtreeWidths.get(parent).getStart(1) + view.getConfig().getHorizontalDistance() * zoomLevel / 2;
                    }
                    if (i > 0)
                    {
                        xCoord.put(node, xOffset);
                    }
                    yCoord.put(node, view.getConfig().getVerticalDistance() * zoomLevel * i + zoomLevel * 3);
                    if (VERBOSE) System.err.println(" X:" + xCoord.get(node) + " Y:" + yCoord.get(node));
                }
                // adapt total tree width to maximum level width (i.e. maximum x
                // position of a node in any level)
                if (nodes.size() > 0)
                {
                    int nodeLevelWidth = xCoord.get(nodes.get(nodes.size() - 1)) 
                                       + nodeWidths.get(nodes.get(nodes.size() - 1)) 
                                       + view.getConfig().getHorizontalDistance() * view.getConfig().getNodeSize();
                    if (nodeLevelWidth > totalTreeWidth)
                    {
                        totalTreeWidth = nodeLevelWidth;
                    }
                }
            }
        }
        if (VERBOSE)
            System.err.println("COMPLETE: Calculate coordinates");
        
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

    //  TODO: make this work also with bottom-up display orientation
    public int getNodeAtCoordinates(int x, int y)
    {
        // binary search over node levels to determine row
        int lowerIndex = 0;
        int upperIndex = nodeLevels.size() - 1;
        int middleIndex = (lowerIndex + upperIndex) / 2;
        int middleBound = yCoord.get(nodeLevels.get(middleIndex).get(0)) + 2;
        if (upperIndex != 0) // simply take the only existing level if there is only one
        {
            while (lowerIndex + 1 < upperIndex)
            {
                // System.err.println("lower: " + lowerIndex + " upper: " +
                // upperIndex + " middle bound: " + middleBound + " y: " + y);
                if (middleBound >= y)
                {
                    upperIndex = middleIndex;
                } else
                {
                    lowerIndex = middleIndex;
                }
                middleIndex = (lowerIndex + upperIndex) / 2;
                middleBound = yCoord.get(nodeLevels.get(middleIndex).get(0)) + 2;
            }
            if (y < middleBound)
                upperIndex--;
        }
        // System.err.println("Node Level: " + upperIndex);

        // binary search over nodes in
        List<Integer> selectedLevel = nodeLevels.get(upperIndex);
        int candidateNode = -1;
        if (selectedLevel.size() == 1)
        {
            candidateNode = selectedLevel.get(0);
        } else
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
                } else
                {
                    lowerIndex = middleIndex;
                }
                middleIndex = (lowerIndex + upperIndex) / 2;
                width = fm.stringWidth(view.getModel().getNodeCaption(selectedLevel.get(middleIndex)));
                middleBound = xCoord.get(selectedLevel.get(middleIndex)) + width / 2 - 2;
                // System.err.println("lower: " + lowerIndex + " upper: " +
                // upperIndex + " middle bound: " + middleBound + " x: " + x);
            }
            if (x < middleBound)
                upperIndex--;
            candidateNode = selectedLevel.get(upperIndex);
        }
        // System.err.println("Potentially clicked node: " + candidateNode);

        // test coordinates against exact boundaries of candidate node
        FontMetrics fm = getFontMetrics(new Font(Font.SANS_SERIF, Font.PLAIN, view.getConfig().getNodeSize()), new BasicStroke(1), view.getConfig().getNodeSize());
        int width = fm.stringWidth(view.getModel().getNodeCaption(candidateNode));
        int xLeft = xCoord.get(candidateNode) - width / 2 - 2;
        int xRight = xLeft + width + 4;
        int yBottom = yCoord.get(candidateNode) + 2;
        int yTop = yBottom - view.getConfig().getNodeSize();
        // System.err.println("test: " + xLeft + " <= " + x + " <= " + xRight +
        // "; " + yTop + " <= " + y + " <= " + yBottom);
        if (xLeft <= x && x <= xRight && yTop <= y && y <= yBottom)
        {
            // System.err.println("Click on node: " + candidateNode);
        } else
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
