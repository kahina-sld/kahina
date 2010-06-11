package org.kahina.core.visual.dag;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Stroke;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.KahinaRunner;
import org.kahina.core.data.dag.KahinaDAG;
import org.kahina.core.data.dag.KahinaMemDAG;
import org.kahina.core.data.tree.KahinaMemTree;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.tree.KahinaTreeView;
import org.kahina.core.visual.tree.KahinaTreeViewMarker;
import org.kahina.core.visual.tree.KahinaTreeViewPanel;
import org.kahina.core.visual.tree.WidthVector;

public class KahinaDAGView extends KahinaView<KahinaDAG>
{
    public static final boolean verbose = false;

    // display options
    private int horizontalDistance = 5;
    private int verticalDistance = 25;
    Color bgColor = Color.WHITE;
    private int antialiasingPolicy = KahinaTreeView.ANTIALIASING;
    
    private int fontSize; // also determines zoom factor
    private int nodeHeight; // do not implement a setter for this, but change it
                            // with font size
    
    //  possible values for antialiasing policy
    public static final int ANTIALIASING = 0;
    public static final int NO_ANTIALIASING = 1;
    
    // layered structure for drawing; also used for reverse indexing
    ArrayList<List<Integer>> nodeLevels;
    Set<Integer> allNodes;
    
    //display coordinates for nodes
    private HashMap<Integer, Integer> nodeX;
    private HashMap<Integer, Integer> nodeY;
    private HashMap<Integer, Integer> nodeHeights;
    private HashMap<Integer, Integer> nodeWidths;


    // special display properties for certain nodes
    HashMap<Integer, Color> nodeBorderColor;

    // mapping from status values to display properties
    HashMap<Integer, Color> statusNodeColorEncoding;
    HashMap<Integer, Color> statusEdgeColorEncoding;
    HashMap<Integer, Color> statusBorderColorEncoding;
    HashMap<Integer, Stroke> statusStrokeEncoding;
    HashMap<Integer, Font> statusFontEncoding;
    
    //allow marking of a single node in the tree
    private int markedNode;

    // hack to allow precalculations from outside any drawing method
    private Graphics2D g;
    
    //private variables for internal calculations across functions
    private int totalTreeHeight;
    private int totalTreeWidth;
    private HashMap<Integer, WidthVector> subtreeWidths;
    private int maxNodeWidth;
    
    public KahinaDAGView()
    {
        model = new KahinaMemDAG();
        resetAllStructures();
        nodeBorderColor = new HashMap<Integer, Color>();
        statusNodeColorEncoding = new HashMap<Integer, Color>();
        statusEdgeColorEncoding = new HashMap<Integer, Color>();
        statusBorderColorEncoding = new HashMap<Integer, Color>();
        statusStrokeEncoding = new HashMap<Integer, Stroke>();
        statusFontEncoding = new HashMap<Integer, Font>();
        horizontalDistance = 10;
        verticalDistance = 2;
        fontSize = 10;
        nodeHeight = 14;

        markedNode = -1;

        maxNodeWidth = 1;
        
        KahinaRunner.getControl().registerListener("update", this);
    }
    
    public void display(KahinaDAG dagModel)
    {
        model = dagModel;
        nodeBorderColor = new HashMap<Integer, Color>();
        recalculate(); // TODO is this necessary?
    }
    
    public void zoomIn()
    {
        if (fontSize < 30)
        {
            fontSize += 1;
        } else
        {
            System.err.println("No zoom levels beyond 30 allowed!");
        }
    }

    public void zoomOut()
    {
        if (fontSize > 6)
        {
            fontSize -= 1;
        } else
        {
            System.err.println("No zoom levels below 6 allowed!");
        }
    }

    public void setZoomLevel(int level)
    {
        fontSize = level;
    }

    public int getZoomLevel()
    {
        return fontSize;
    }

    public int getHorizontalDistance()
    {
        return horizontalDistance;
    }

    public void setHorizontalDistance(int horizontalDistance)
    {
        this.horizontalDistance = horizontalDistance;
    }

    public void decreaseHorizontalDistance()
    {
        if (horizontalDistance > 2)
        {
            horizontalDistance -= 1;
        } else
        {
            System.err.println("No horizontal distance values under 2 allowed!");
        }
    }

    public void increaseHorizontalDistance()
    {
        if (horizontalDistance < 20)
        {
            horizontalDistance += 1;
        } else
        {
            System.err.println("No horizontal distance values over 20 allowed!");
        }
    }
    
    public int getVerticalDistance()
    {
        return verticalDistance;
    }

    public void setVerticalDistance(int verticalDistance)
    {
        this.verticalDistance = verticalDistance;
    }

    public void decreaseVerticalDistance()
    {
        if (verticalDistance > 2)
        {
            verticalDistance -= 1;
        } else
        {
            System.err.println("No vertical distance values under 2 allowed!");
        }
    }

    public void increaseVerticalDistance()
    {
        if (verticalDistance < 20)
        {
            verticalDistance += 1;
        } else
        {
            System.err.println("No vertical distance values over 20 allowed!");
        }
    }
    
    public int getAntialiasingPolicy()
    {
        return antialiasingPolicy;
    }

    public void setAntialiasingPolicy(int newPolicy)
    {
        if (newPolicy >= 0 && newPolicy <= 1)
        {
            antialiasingPolicy = newPolicy;
        } else
        {
            System.err.println("WARNING: unknown antialiasing policy value " + newPolicy);
        }
    }
    
    public int getDisplayWidth()
    {
        return totalTreeWidth;
    }

    public int getDisplayHeight()
    {
        return totalTreeHeight;
    }

    public Font getNodeFont(int nodeID)
    {
        int status = model.getNodeStatus(nodeID);
        Font fnt = statusFontEncoding.get(status);
        if (fnt == null)
        {
            if (model.isCollapsed(nodeID))
            {
                return new Font(Font.SANS_SERIF, Font.BOLD, fontSize);
            }
            return new Font(Font.SANS_SERIF, Font.PLAIN, fontSize);
        }
        else
        {
            return new Font(fnt.getFamily(), fnt.getStyle(), fontSize);
        }
    }
    
    public Color getNodeColor(int nodeID)
    {
        int status = model.getNodeStatus(nodeID);
        Color col = statusNodeColorEncoding.get(status);
        // System.err.println("Node " + nodeID + ": status " + status +
        // " ->  color " + col);
        if (col == null)
        {
            return Color.WHITE;
        } else
        {
            return col;
        }
    }

    public void setNodeBorderColor(int nodeID, Color color)
    {
        if (color == null)
        {
            nodeBorderColor.remove(nodeID);
        } else
        {
            nodeBorderColor.put(nodeID, color);
        }
    }

    public Color getNodeBorderColor(int nodeID)
    {
        return nodeBorderColor.get(nodeID);
    }
    
    public int getNodeX(int nodeID)
    {
        return nodeX.get(nodeID);
    }

    public int getNodeY(int nodeID)
    {
        return nodeY.get(nodeID);
    }
    
    public int getNodeWidth(int nodeID)
    {
        return nodeWidths.get(nodeID);
    }

    public int getNodeHeight(int nodeID)
    {
        return nodeHeights.get(nodeID);
    }
    
    public void setStatusColorEncoding(int status, Color color)
    {
        statusNodeColorEncoding.put(status, color);
    }

    public void setStatusFontEncoding(int status, Font font)
    {
        statusFontEncoding.put(status, font);
    }

    public int getMarkedNode()
    {
        return markedNode;
    }

    public void setMarkedNode(int markedNode)
    {
        this.markedNode = markedNode;
    }

    public void resetAllStructures()
    {
        nodeLevels = new ArrayList<List<Integer>>();
        allNodes = new HashSet<Integer>();

        nodeX = new HashMap<Integer, Integer>();
        nodeY = new HashMap<Integer, Integer>();
        nodeWidths = new HashMap<Integer, Integer>();
        nodeHeights = new HashMap<Integer, Integer>();

        subtreeWidths = new HashMap<Integer, WidthVector>();
    }
    
    private void createNodeLayers()
    {
        if (verbose)
            System.err.println("BEGIN: Create node layers");
        int rootID = model.getRootID();
        ArrayList<Integer> rootLevel = new ArrayList<Integer>();
        if (rootID != -1)
        {
            rootLevel.add(rootID);
            nodeLevels.add(rootLevel);
            allNodes.addAll(rootLevel);
            List<Integer> children = getVisibleVirtualDescendants(model, model.getRootID());
            while (true)
            {
                if (verbose) System.err.println("children:" + children);
                ArrayList<Integer> grandchildren = new ArrayList<Integer>();
                for (int i = 0; i < children.size(); i++)
                {
                    grandchildren.addAll(getVisibleVirtualDescendants(model, children.get(i)));
                }
                nodeLevels.add(children);
                allNodes.addAll(children);
                children = grandchildren;
                if (grandchildren.size() == 0)
                {
                    break;
                }
            }
        }
        if (verbose) System.err.println("COMPLETE: Create node layers");
        if (verbose) System.err.println("Levels:\n" + showLevels());
    }
    
    public boolean displaysNode(int nodeID)
    {
        return (nodeX.get(nodeID) != null);
    }

    public boolean nodeIsVisible(int nodeID)
    {
        // TODO: implement special DAG collapsing behavior
        //if (model.hasCollapsedAncestor(nodeID)) return false;
        return true;
    }
    
    private ArrayList<Integer> getVisibleVirtualDescendants(KahinaDAG dagModel, int nodeID)
    {
        ArrayList<Integer> descendants = new ArrayList<Integer>();
        // System.err.println("\t Actual children for node " + nodeID + ": " +
        // treeModel.getChildren(nodeID,treeLayer));
        //TODO: implement special DAG collapsing behavior
        //if (dagModel.isCollapsed(nodeID)) return descendants;
        List<Integer> outgoingEdges = dagModel.getOutgoingEdges(nodeID);
        for (int outgoing : outgoingEdges)
        {
            descendants.add(dagModel.getEndNode(outgoing));
        }
        if (verbose)
        {
            System.err.println("Breadth-first search for visible descendants of " + nodeID + ":");
        }
        for (int i = 0; i < descendants.size(); i++)
        {
            boolean nodeIsVisible = nodeIsVisible(descendants.get(i));
            if (verbose)
            {
                System.err.print(descendants.get(i));
                System.err.print(nodeIsVisible ? "+" : "-");
            }
            if (!nodeIsVisible)
            {
                List<Integer> edges = dagModel.getOutgoingEdges(descendants.remove(i));
                for (int edge : edges)
                {
                    descendants.add(dagModel.getEndNode(edge));
                }
                i--;
            }
        }
        if (verbose)
        {
            System.err.println();
            System.err.println("Virtual children for node " + nodeID + ": " + descendants);
        }
        return descendants;
    }
    

    private WidthVector constructWidthVector(ArrayList<Integer> children)
    {
        if (children.size() > 0)
        {
            WidthVector sum = subtreeWidths.get(children.get(0)).copy();
            for (int i = 1; i < children.size(); i++)
            {
                sum = WidthVector.adjoin(sum, subtreeWidths.get(children.get(i)));
            }
            sum.start.add(0, 1);
            sum.end.add(0, 1);
            return sum;
        }
        return new WidthVector();
    }

    public FontMetrics getFontMetrics(Font f, Stroke s, int fontSize)
    {
        // hack to allow precalculations from outside any drawing method
        if (g == null)
        {
            BufferedImage bufferedImage = new BufferedImage(2, 2, BufferedImage.TYPE_4BYTE_ABGR_PRE);
            g = (Graphics2D) bufferedImage.createGraphics();
        }
        g.setFont(new Font(f.getFontName(), f.getStyle(), fontSize));
        g.setStroke(s);
        return g.getFontMetrics();
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
    public int nodeAtCoordinates(int x, int y)
    {
        // binary search over node levels to determine row
        int lowerIndex = 0;
        int upperIndex = nodeLevels.size() - 1;
        int middleIndex = (lowerIndex + upperIndex) / 2;
        int middleBound = nodeY.get(nodeLevels.get(middleIndex).get(0)) + 2;
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
                middleBound = nodeY.get(nodeLevels.get(middleIndex).get(0)) + 2;
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
            FontMetrics fm = getFontMetrics(new Font(Font.SANS_SERIF, Font.PLAIN, fontSize), new BasicStroke(1), fontSize);
            int width = fm.stringWidth(model.getNodeCaption(selectedLevel.get(middleIndex)));
            middleBound = nodeX.get(selectedLevel.get(middleIndex)) + width / 2 + 2;
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
                width = fm.stringWidth(model.getNodeCaption(selectedLevel.get(middleIndex)));
                middleBound = nodeX.get(selectedLevel.get(middleIndex)) + width / 2 - 2;
                // System.err.println("lower: " + lowerIndex + " upper: " +
                // upperIndex + " middle bound: " + middleBound + " x: " + x);
            }
            if (x < middleBound)
                upperIndex--;
            candidateNode = selectedLevel.get(upperIndex);
        }
        // System.err.println("Potentially clicked node: " + candidateNode);

        // test coordinates against exact boundaries of candidate node
        FontMetrics fm = getFontMetrics(new Font(Font.SANS_SERIF, Font.PLAIN, fontSize), new BasicStroke(1), fontSize);
        int width = fm.stringWidth(model.getNodeCaption(candidateNode));
        int xLeft = getNodeX(candidateNode) - width / 2 - 2;
        int xRight = xLeft + width + 4;
        int yBottom = getNodeY(candidateNode) + 2;
        int yTop = yBottom - getNodeHeight(candidateNode);
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

    public KahinaDAG getDAGModel()
    {
        return model;
    }

    public JComponent wrapInPanel()
    {
        KahinaDAGViewPanel panel = new KahinaDAGViewPanel();
        KahinaRunner.getControl().registerListener("redraw", panel);
        panel.setView(this);
        JScrollPane scrollPane = new JScrollPane(panel);
        scrollPane.getViewport().setBackground(bgColor);
        return scrollPane;
    }

    public void recalculate()
    {
        resetAllStructures();
        calculateCoordinates();
        // System.err.println("Levels:\n" + showLevels());
    }
    
    public void calculateCoordinates()
    {
        if (verbose) System.err.println("BEGIN: Calculate coordinates");
        // node height determined by font size
        FontMetrics fm = getFontMetrics(new Font(Font.SANS_SERIF, Font.PLAIN, fontSize), new BasicStroke(1), fontSize);
        nodeHeight = fm.getHeight();

        createNodeLayers();
        // System.err.println(showLevels());
        // System.err.println(terminalLayer);

        totalTreeWidth = 50;
        totalTreeHeight = (nodeLevels.size() + 2) * verticalDistance * fontSize + 10;

        if (model.getRootID() != -1)
        {
            if (verbose) System.err.println("BEGIN: Calculate subtree widths");
            // calculate (maximum) subtree width for each node bottom-up
            for (int i = nodeLevels.size() - 1; i >= 0; i--)
            {
                if (verbose) System.err.println("Node level: " + i);
                for (int node : nodeLevels.get(i))
                {
                    Dimension nodeDimension = computeNodeDimension(fm, node);
                    nodeWidths.put(node, nodeDimension.width);
                    nodeHeights.put(node, nodeDimension.height);
                    // System.err.println("labelWidth(" +
                    // getContentfulTreeModel().getNodeCaption(node) + ") = " +
                    // nodeLabelWidth);
                    if (maxNodeWidth < nodeDimension.width) maxNodeWidth = nodeDimension.width;
                    ArrayList<Integer> children = getVisibleVirtualDescendants(model, node);
                    subtreeWidths.put(node, constructWidthVector(children));
                    if (verbose)  System.err.println("  Node:" + node + " VisChildren:" + children + " WidthVector:" + subtreeWidths.get(node));
                }
            }
            if (verbose) System.err.println("COMPLETE: Calculate subtree widths");
            if (verbose) System.err.println("maxNodeWidth = " + maxNodeWidth);
            nodeX.put(model.getRootID(), subtreeWidths.get(model.getRootID()).maximumLeftDistance() * horizontalDistance * fontSize / 2);
            for (int i = 0; i < nodeLevels.size(); i++)
            {
                if (verbose) System.err.println("Node level: " + i);
                List<Integer> nodes = nodeLevels.get(i);
                int xOffset = 0;
                if (nodes.size() > 0) xOffset = subtreeWidths.get(nodes.get(0)).maximumLeftDistance() * horizontalDistance * fontSize / 2;
                int parent = -1;
                WidthVector subtreeWidth = new WidthVector();
                WidthVector lastSubtreeWidth;
                for (int node : nodes)
                {
                    if (verbose) System.err.print("  Node:" + node);
                    lastSubtreeWidth = subtreeWidth;
                    subtreeWidth = subtreeWidths.get(node);
                    xOffset += WidthVector.computeNecessaryDistance(lastSubtreeWidth, subtreeWidth) * horizontalDistance * fontSize / 2;
                    // switch to children of next parent node --> jump in x offset
                    int newParent = getVisibleMiddleAntecedent(node);
                    if (verbose) System.err.print(" VisParent:" + newParent);
                    if (i > 0 && newParent != parent)
                    {
                        parent = newParent;
                        if (verbose) System.err.print(" SubtreeWidths:" + subtreeWidths.get(parent));
                        // old variant of xOffset computation
                        // xOffset = (int)((nodeX.get(parent) - (subtreeWidths.get(parent).getStart(1) * 0.5 - 0.5) * horizontalDistance * fontSize));
                        xOffset = nodeX.get(parent) - subtreeWidths.get(parent).getStart(1) / 2 * horizontalDistance * fontSize / 2;
                    }
                    if (i > 0)
                    {
                        nodeX.put(node, xOffset);
                    }
                    nodeY.put(node, verticalDistance * fontSize * i + fontSize * 3);
                    if (verbose) System.err.println(" X:" + nodeX.get(node) + " Y:" + nodeY.get(node));
                }
                // adapt total tree width to maximum level width (i.e. maximum x
                // position of a node in any level)
                if (nodes.size() > 0)
                {
                    int nodeLevelWidth = nodeX.get(nodes.get(nodes.size() - 1)) + horizontalDistance * fontSize;
                    if (nodeLevelWidth > totalTreeWidth)
                    {
                        totalTreeWidth = nodeLevelWidth;
                    }
                }
            }
        }
        if (verbose)
            System.err.println("COMPLETE: Calculate coordinates");
    }
    
    private Dimension computeNodeDimension(FontMetrics fm, int nodeID)
    {
        int maxWidth = 0;
        String tag = model.getNodeCaption(nodeID);
        String[] stringParts = tag.split("\\\\n");
        for (String part : stringParts)
        {
            int width = fm.stringWidth(part);
            if (width > maxWidth) maxWidth = width;
        }        
        return new Dimension(maxWidth, stringParts.length * nodeHeight + 4);
    }
    
    private int getVisibleMiddleAntecedent(int nodeID)
    {
        List<Integer> antecedents = model.getIncomingEdges(nodeID);
        if (antecedents.size() == 0) return -1;
        int middleIndex = antecedents.size() / 2;
        return model.getStartNode(middleIndex);
    }
    
    protected void processEvent(KahinaUpdateEvent e)
    {
        // recalculation is implicitly part of this (via marker)
        markedNode = e.getSelectedStep();
        this.recalculate();
    }
}
