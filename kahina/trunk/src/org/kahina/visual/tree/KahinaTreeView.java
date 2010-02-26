package org.kahina.visual.tree;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Stroke;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.kahina.data.KahinaObject;
import org.kahina.data.KahinaTypeException;
import org.kahina.data.tree.KahinaTree;
import org.kahina.data.tree.KahinaMemTree;
import org.kahina.visual.KahinaView;

public class KahinaTreeView extends KahinaView
{
    // display options
    private int horizontalDistance = 5; 
    private int verticalDistance = 25;
    Color bgColor = Color.WHITE;
    private int nodeShapePolicy = KahinaTreeView.BOX_SHAPE;
    private int edgeShapePolicy = KahinaTreeView.OVAL_SHAPE;
    private int nodeDisplayPolicy = KahinaTreeView.STATUS_DEFAULT_YES;
    private int collapsePolicy = KahinaTreeView.COLLAPSE_SECONDARY;
    private int terminalsPolicy = KahinaTreeView.NO_SPECIAL_TREATMENT;
    private int lineShapePolicy = KahinaTreeView.INVISIBLE_LINES;
    private int secondaryLineShapePolicy = KahinaTreeView.EDGY_LINES;
    private int nodePositionPolicy = KahinaTreeView.LEFT_ALIGNED_NODES;
    private int antialiasingPolicy = KahinaTreeView.ANTIALIASING;
    private int displayOrientation = KahinaTreeView.TOP_DOWN_DISPLAY;
    private boolean displaySecondDimension = true;
    HashMap<Integer,Boolean> statusDisplayed;
    private int fontSize; //also determines zoom factor
    private int nodeHeight; //do not implement a setter for this, but change it with font size
        
    //DISPLAY CONSTANTS
    
    //possible values for shape policies
    public static final int BOX_SHAPE = 0;
    public static final int OVAL_SHAPE = 1;
    
    //possible values for node display policy
    public static final int ALWAYS = 0;
    public static final int STATUS_DEFAULT_YES = 1;
    public static final int STATUS_DEFAULT_NO = 2;
    public static final int NEVER = 3;
    public static final int CONDITIONALLY = 4; //consults an additional user-definable function to decide  
    
    //possible values for collapsing policy
    public static final int NO_COLLAPSING = 0;
    public static final int COLLAPSE_PRIMARY = 1;
    public static final int COLLAPSE_SECONDARY = 2;
    
    //possible values for terminals policy
    public static final int NO_SPECIAL_TREATMENT = 0;
    public static final int ON_EXTRA_LEVEL = 1;
    public static final int GRAPHICALLY_SEPARATED = 2;
    
    //possible values for line display policy
    public static final int STRAIGHT_LINES = 0;
    public static final int EDGY_LINES = 1;
    public static final int INVISIBLE_LINES = 2;
    
    //possible values for displayOrientation
    public static final int TOP_DOWN_DISPLAY = 0;
    public static final int BOTTOM_UP_DISPLAY = 1;
    
    //possible values for node position policy
    public static final int CENTERED_NODES = 0;
    public static final int LEFT_ALIGNED_NODES = 1;
    public static final int RIGHT_ALIGNED_NODES= 2;
    
    //possible values for antialiasing policy
    public static final int ANTIALIASING = 0;
    public static final int NO_ANTIALIASING = 1;
    
    //possible values for line types
    public static final int COMPLETE_LINES = 0;
    public static final int DOTTED_LINES = 1;
    
    KahinaTree treeModel;
    int treeLayer = 0;
    //layered structure for drawing; also used for reverse indexing
    ArrayList<List<Integer>> nodeLevels;
    
    //is usually null; add another model here for two-dimensional display
    KahinaTree secondaryTreeModel;
    
    private boolean dimensionsSwapped = false;
    
    //display coordinates for nodes
    private HashMap<Integer, Integer> nodeX;
    private HashMap<Integer, Integer> nodeY;
    
    //special display properties for certain nodes
    HashMap<Integer, Color> nodeBorderColor;
    
    //mapping from status values to display properties
    HashMap<Integer, Color> statusNodeColorEncoding;
    HashMap<Integer, Color> statusEdgeColorEncoding;
    HashMap<Integer, Color> statusBorderColorEncoding;
    HashMap<Integer, Stroke> statusStrokeEncoding;
    HashMap<Integer, Font> statusFontEncoding;
    HashMap<Integer, Boolean> statusVisibilityEncoding;
    
    //TODO: generalize this concept, via status if possible
    //allow marking of a single node in the tree
    private int markedNode;
    
    //hack to allow precalculations from outside any drawing method
    private Graphics2D g;
    
    //private variables for internal calculations across functions
    private int totalTreeHeight;
    private int totalTreeWidth;
    private HashMap<Integer,WidthVector> subtreeWidths;
    private ArrayList<Integer> terminalLayer;
    private int maxNodeWidth;
    
    public KahinaTreeView()
    {
        treeModel = new KahinaMemTree();
        treeLayer = 0;
        secondaryTreeModel = null;
        resetAllStructures();
        nodeBorderColor = new HashMap<Integer, Color>();
        statusNodeColorEncoding = new HashMap<Integer, Color>();
        statusEdgeColorEncoding = new HashMap<Integer, Color>();
        statusBorderColorEncoding = new HashMap<Integer, Color>();
        statusStrokeEncoding = new HashMap<Integer, Stroke>();
        statusFontEncoding = new HashMap<Integer, Font>();
        statusVisibilityEncoding = new HashMap<Integer, Boolean>();
        horizontalDistance = 10;
        verticalDistance = 2;
        fontSize = 10;
        nodeHeight = 14;
        
        markedNode = -1;
        
        maxNodeWidth = 1;
    }
    
    public void zoomIn()
    {
        if (fontSize < 30)
        {
            fontSize += 1;
            resetAllStructures();
            calculateCoordinates();
        }
        else
        {
            System.err.println("No zoom levels beyond 30 allowed!");
        }
    }
    
    public void zoomOut()
    {
        if (fontSize > 6)
        {
            fontSize -= 1;
            resetAllStructures();
            calculateCoordinates();
        }
        else
        {
            System.err.println("No zoom levels below 6 allowed!");
        }
    }
    
    public void setZoomLevel(int level)
    {
        fontSize = level;
        resetAllStructures();
        calculateCoordinates();
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
        resetAllStructures();
        calculateCoordinates();
    }
    
    public void decreaseHorizontalDistance()
    {
        if (horizontalDistance > 2)
        {
            horizontalDistance -= 1;
            resetAllStructures();
            calculateCoordinates();
        }
        else
        {
            System.err.println("No horizontal distance values under 2 allowed!");
        }
    }
    
    public void increaseHorizontalDistance()
    {
        if (horizontalDistance < 20)
        {
            horizontalDistance += 1;
            resetAllStructures();
            calculateCoordinates();
        }
        else
        {
            System.err.println("No horizontal distance values over 20 allowed!");
        }
    }
    
    public boolean isSecondDimensionDisplayed()
    {
        return displaySecondDimension && secondaryTreeModel != null;
    }
    
    public void toggleSecondDimensionDisplay()
    {
        displaySecondDimension = !displaySecondDimension;
        resetAllStructures();
        calculateCoordinates();
    }

    public int getVerticalDistance()
    {
        return verticalDistance;
    }

    public void setVerticalDistance(int verticalDistance)
    {
        this.verticalDistance = verticalDistance;
        resetAllStructures();
        calculateCoordinates();
    }
    
    public void decreaseVerticalDistance()
    {
        if (verticalDistance > 2)
        {
            verticalDistance -= 1;
            resetAllStructures();
            calculateCoordinates();
        }
        else
        {
            System.err.println("No vertical distance values under 2 allowed!");
        }
    }
    
    public void increaseVerticalDistance()
    {
        if (verticalDistance < 20)
        {
            verticalDistance += 1;
            resetAllStructures();
            calculateCoordinates();
        }
        else
        {
            System.err.println("No vertical distance values over 20 allowed!");
        }
    }

    public int getCollapsePolicy()
    {
        return collapsePolicy;
    }

    public void setCollapsePolicy(int collapsePolicy)
    {
        if (collapsePolicy >= 0 && collapsePolicy <= 2)
        {
            this.collapsePolicy = collapsePolicy;
            resetAllStructures();
            calculateCoordinates();
        }
        else
        {
            System.err.println("WARNING: unknown collapse policy value " + collapsePolicy);
        }
    }

    public int getDisplayOrientation()
    {
        return displayOrientation;
    }

    public void setDisplayOrientation(int newPolicy)
    {
        if (newPolicy >= 0 && newPolicy <= 1)
        {
            displayOrientation = newPolicy;
        }
        else
        {
            System.err.println("WARNING: unknown displayOrientation value " + newPolicy);
        }
    }

    public int getEdgeShapePolicy()
    {
        return edgeShapePolicy;
    }

    public void setEdgeShapePolicy(int newPolicy)
    {
        if (newPolicy >= 0 && newPolicy <= 1)
        {
            edgeShapePolicy = newPolicy;
        }
        else
        {
            System.err.println("WARNING: unknown edge shape policy value " + newPolicy);
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
        }
        else
        {
            System.err.println("WARNING: unknown antialiasing policy value " + newPolicy);
        }
    }

    public int getLineShapePolicy()
    {
        return lineShapePolicy;
    }

    public void setLineShapePolicy(int newPolicy)
    {
        if (newPolicy >= 0 && newPolicy <= 2)
        {
            lineShapePolicy = newPolicy;
        }
        else
        {
            System.err.println("WARNING: unknown line shape policy value " + newPolicy);
        }
    }
    
    public int getSecondaryLineShapePolicy()
    {
        return secondaryLineShapePolicy;
    }

    public void setSecondaryLineShapePolicy(int newPolicy)
    {
        if (newPolicy >= 0 && newPolicy <= 2)
        {
            secondaryLineShapePolicy = newPolicy;
        }
        else
        {
            System.err.println("WARNING: unknown line shape policy value " + newPolicy);
        }
    }

    public int getNodeDisplayPolicy()
    {
        return nodeDisplayPolicy;
    }

    public void setNodeDisplayPolicy(int newPolicy)
    {
        if (newPolicy >= 0 && newPolicy <= 4)
        {
            nodeDisplayPolicy = newPolicy;
            resetAllStructures();
            calculateCoordinates();
        }
        else
        {
            System.err.println("WARNING: unknown node display policy value " + newPolicy);
        }
    }

    public int getNodePositionPolicy()
    {
        return nodePositionPolicy;
    }

    public void setNodePositionPolicy(int newPolicy)
    {
        if (newPolicy >= 0 && newPolicy <= 2)
        {
            nodePositionPolicy = newPolicy;
        }
        else
        {
            System.err.println("WARNING: unknown node position policy value " + newPolicy);
        }
    }

    public int getNodeShapePolicy()
    {
        return nodeShapePolicy;
    }

    public void setNodeShapePolicy(int newPolicy)
    {
        if (newPolicy >= 0 && newPolicy <= 1)
        {
            nodeShapePolicy = newPolicy;
        }
        else
        {
            System.err.println("WARNING: unknown node shape policy value " + newPolicy);
        }
    }

    public int getTerminalsPolicy()
    {
        return terminalsPolicy;
    }

    public void setTerminalsPolicy(int newPolicy)
    {
        if (newPolicy >= 0 && newPolicy <= 2)
        {
            terminalsPolicy = newPolicy;
            resetAllStructures();
            calculateCoordinates();
        }
        else
        {
            System.err.println("WARNING: unknown terminals policy value " + newPolicy);
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
        int status = getContentfulTreeModel().getNodeStatus(nodeID);
        Font fnt = statusFontEncoding.get(status);
        if (fnt == null)
        {
            if (treeModel.isCollapsed(nodeID) && collapsePolicy == COLLAPSE_PRIMARY)
            {
                return new Font(Font.SANS_SERIF,Font.BOLD, fontSize);
            }
            if (secondaryTreeModel != null && secondaryTreeModel.isCollapsed(nodeID) && collapsePolicy == COLLAPSE_SECONDARY)
            {
                return new Font(Font.SANS_SERIF,Font.BOLD, fontSize);
            }
            return new Font(Font.SANS_SERIF,Font.PLAIN, fontSize);
        }
        else
        {
            return new Font(fnt.getFamily(), fnt.getStyle(), fontSize);
        }
    }
    
    public Color getNodeColor(int nodeID)
    {
        int status = getContentfulTreeModel().getNodeStatus(nodeID);
        Color col = statusNodeColorEncoding.get(status);
        if (col == null)
        {
            return Color.WHITE;
        }
        else
        {
            return col;
        }
    }
    
    public void setNodeBorderColor(int nodeID, Color color)
    {
        if (color == null)
        {
            nodeBorderColor.remove(nodeID);
        }
        else
        {
            nodeBorderColor.put(nodeID, color);
        }    
    }
    
    public Color getNodeBorderColor(int nodeID)
    {
        return nodeBorderColor.get(nodeID);
    }
    
    public int getEdgeStyle(int nodeID)
    {
        return COMPLETE_LINES;
    }
    
    public int getNodeX(int nodeID)
    {
        return nodeX.get(nodeID);
    }
    
    public int getNodeY(int nodeID)
    {
        if (displayOrientation == BOTTOM_UP_DISPLAY)
        {
            return totalTreeHeight - nodeY.get(nodeID);
        }
        return nodeY.get(nodeID);
    }
    
    public int getNodeHeight(int nodeID)
    {
        return nodeHeight;
    }
    
    public int getTreeLayer()
    {
        return treeLayer;
    }

    public void setTreeLayer(int treeLayer)
    {
        this.treeLayer = treeLayer;
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
        statusDisplayed = new HashMap<Integer,Boolean>();
        
        nodeLevels = new ArrayList<List<Integer>>();
        
        nodeX = new HashMap<Integer, Integer>();
        nodeY = new HashMap<Integer, Integer>();
        
        subtreeWidths = new HashMap<Integer,WidthVector>();
    }
    
    public void display(KahinaObject treeModel) throws KahinaTypeException
    {
        treeLayer = 0;
        this.treeModel = (KahinaTree) treeModel;
        nodeBorderColor = new HashMap<Integer, Color>();
        resetAllStructures();
        calculateCoordinates();
    }
    
    public void displaySecondaryTree(KahinaObject treeModel) throws KahinaTypeException
    {
        this.secondaryTreeModel = (KahinaTree) treeModel;
        ((KahinaTree) this.secondaryTreeModel).setReferenceNode(((KahinaTree) this.treeModel).getReferenceNode());
        this.secondaryTreeModel.setPrimaryModel(this.treeModel);
        nodeBorderColor = new HashMap<Integer, Color>();
        resetAllStructures();
        calculateCoordinates();
    }
    
    public void display(KahinaTree layerModel, int layerID, int referenceNode) throws KahinaTypeException
    {
        treeLayer = layerID;
        layerModel.setReferenceNode(referenceNode);
        this.treeModel = layerModel;
        nodeBorderColor = new HashMap<Integer, Color>();
        resetAllStructures();
        calculateCoordinates();
    }
    
    public void calculateCoordinates()
    {
        //System.err.println("BEGIN: Calculate coordinates for layer " + treeLayer);
        //node height determined by font size       
        FontMetrics fm = getFontMetrics(new Font(Font.SANS_SERIF,Font.PLAIN, fontSize), new BasicStroke(1), fontSize);
        nodeHeight = fm.getHeight();

        createNodeLayers();
        //System.err.println(showLevels());
        //System.err.println(terminalLayer);
        
        totalTreeWidth = 50;
        totalTreeHeight = (nodeLevels.size() + 2) * verticalDistance * fontSize;      
             
        if (treeModel.getRootID(treeLayer) != -1)
        {
            //System.err.println("BEGIN: Calculate subtree widths for layer " + treeLayer);
            //calculate (maximum) subtree width for each node bottom-up
            if (terminalsPolicy != NO_SPECIAL_TREATMENT)
            {
                for (int node : terminalLayer)
                {
                    subtreeWidths.put(node,constructTerminalWidthVector());
                }
            }
            for (int i = nodeLevels.size() - 1; i >= 0; i--)
            {
                //System.err.println("Node level: " + i);
                for (int node : nodeLevels.get(i))
                {
                    int nodeLabelWidth = fm.stringWidth(getContentfulTreeModel().getNodeCaption(node));
                    if (maxNodeWidth < nodeLabelWidth) maxNodeWidth = nodeLabelWidth;
                    ArrayList<Integer> children = getVisibleVirtualChildren(treeModel, node);
                    subtreeWidths.put(node,constructWidthVector(children));
                    //System.err.println("  Node:" + node + " VisChildren:" + children + " WidthVector:" + subtreeWidths.get(node));
                }
            }
            //System.err.println("COMPLETE: Calculate subtree widths for layer " + treeLayer);
            //System.err.println("maxNodeWidth = " + maxNodeWidth);
            nodeX.put(treeModel.getRootID(treeLayer), subtreeWidths.get(treeModel.getRootID(treeLayer)).maximumLeftDistance() * horizontalDistance * fontSize / 2);
            for (int i = 0; i < nodeLevels.size(); i++)
            {
                //System.err.println("Node level: " + i);
                List<Integer> nodes = nodeLevels.get(i);  
                int xOffset = 0;
                if (nodes.size() > 0) xOffset = subtreeWidths.get(nodes.get(0)).maximumLeftDistance() * horizontalDistance * fontSize / 2;
                //TODO: find out why this does not seem to have any effect
                if (nodePositionPolicy == CENTERED_NODES)
                {
                    xOffset += maxNodeWidth;
                }
                if (nodePositionPolicy == RIGHT_ALIGNED_NODES)
                {
                    xOffset += maxNodeWidth;
                }
                int parent = -1;
                WidthVector subtreeWidth  = new WidthVector();
                WidthVector lastSubtreeWidth;
                for (int node : nodes)
                {
                    //System.err.print("  Node:" + node);
                    lastSubtreeWidth = subtreeWidth;
                    subtreeWidth = subtreeWidths.get(node);
                    xOffset += WidthVector.computeNecessaryDistance(lastSubtreeWidth, subtreeWidth) * horizontalDistance * fontSize / 2;
                    //switch to children of next parent node --> jump in x offset
                    int newParent = getVisibleParent(node);
                    //System.err.print(" VisParent:" + newParent);
                    if (i > 0 && newParent != parent)
                    {
                        parent = newParent;
                        //System.err.print(" SubtreeWidths:" + subtreeWidths.get(parent));
                        xOffset = (int)((nodeX.get(parent) -  (subtreeWidths.get(parent).getStart(1) * 0.5  - 0.5) * horizontalDistance * fontSize));
                    }
                    if (i > 0)
                    {
                        nodeX.put(node, xOffset);
                    }
                    nodeY.put(node, verticalDistance * fontSize * i + fontSize * 3);
                    //System.err.println(" X:" + nodeX.get(node) + " Y:" + nodeY.get(node));
                }
                //adapt total tree width to maximum level width (i.e. maximum x position of a node in any level)     
                if (nodes.size() > 0)
                {
                    int nodeLevelWidth =  nodeX.get(nodes.get(nodes.size() - 1)) + horizontalDistance * fontSize;
                    if (nodeLevelWidth > totalTreeWidth)
                    {
                       totalTreeWidth = nodeLevelWidth;
                    }
                }
            }
            //position terminals
            nodeLevels.get(nodeLevels.size() - 1).addAll(terminalLayer);
            for (int i : terminalLayer)
            {
                nodeX.put(i, nodeX.get(treeModel.getParent(i,treeLayer)));
                nodeY.put(i, (nodeLevels.size() + 1) * verticalDistance * fontSize);
            }
            //move nodes around according to secondary tree structure and policy
            if (secondaryTreeModel != null && displaySecondDimension)
            {
                //System.err.println("BEGIN: adapt X values to secondary tree model");
                int depth = 0;
                ArrayList<Integer> agenda = new ArrayList<Integer>();
                agenda.add(secondaryTreeModel.getRootID(treeLayer));
                while (agenda.size() > 0)
                {
                    //System.err.println("Agenda: " + agenda);
                    int s = agenda.size();
                    for (int i = 0; i < s; i++)
                    {
                        int nodeID = agenda.remove(0);
                        if (nodeX.get(nodeID) != null)
                        {
                            if (nodePositionPolicy == KahinaTreeView.RIGHT_ALIGNED_NODES)
                            {
                                nodeX.put(nodeID, nodeX.get(nodeID) - depth * fontSize);
                            }
                            else
                            {
                                nodeX.put(nodeID, nodeX.get(nodeID) + depth * fontSize);
                            }
                            //System.err.println(" depth(" + nodeID + ") = " + depth);
                            agenda.addAll(getVisibleVirtualChildren(secondaryTreeModel,nodeID));
                        }
                    }
                    depth++;
                }
                //System.err.println("COMPLETE: adapt X values to secondary tree model");
            }
        }
        //System.err.println("COMPLETE: Calculate coordinates for layer " + treeLayer);
    }
    
    private void createNodeLayers()
    {
      //System.err.println("BEGIN: Create node layers for layer " + treeLayer);
      terminalLayer = new ArrayList<Integer>();
      int rootID = treeModel.getRootID(treeLayer);
      ArrayList<Integer> rootLevel = new ArrayList<Integer>();
      if (rootID != -1)
      {
          rootLevel.add(rootID);
          nodeLevels.add(rootLevel);
          List<Integer> children = getVisibleVirtualChildren(treeModel, treeModel.getRootID(treeLayer));
          while(true)
          {
            //System.err.println("children:" + children);
            ArrayList<Integer> grandchildren = new ArrayList<Integer>();
            for (int i = 0; i < children.size(); i++)
            {
                //terminal handling here
                if (terminalsPolicy != NO_SPECIAL_TREATMENT && getVisibleVirtualChildren(treeModel, children.get(i)).isEmpty())
                {         
                    terminalLayer.add(children.remove(i));
                    i--;
                }
                else
                {
                    grandchildren.addAll(getVisibleVirtualChildren(treeModel, children.get(i)));
                }
            }
            nodeLevels.add(children);
            children = grandchildren;
            if (grandchildren.size() == 0)
            {
                break;
            }
          }
      }
      //System.err.println("COMPLETE: Create node layers for layer " + treeLayer);
    }
    
    public boolean displaysNode(int nodeID)
    {
        return (nodeX.get(nodeID) != null);
    }
    
    public boolean nodeIsVisible(int nodeID)
    {
        //TODO: process conditional option, allow user-definable decision function
        if (nodeDisplayPolicy == KahinaTreeView.ALWAYS) return true;
        if (nodeDisplayPolicy == KahinaTreeView.NEVER) return false;
        if (secondaryTreeModel != null && collapsePolicy == COLLAPSE_SECONDARY && secondaryTreeModel.hasCollapsedAncestor(nodeID)) return false;
        int status = getContentfulTreeModel().getNodeStatus(nodeID);
        Boolean decision = statusVisibilityEncoding.get(status);
        if (decision == null)
        {
            //default values decide
            if (nodeDisplayPolicy == KahinaTreeView.STATUS_DEFAULT_YES) return true;
            if (nodeDisplayPolicy == KahinaTreeView.STATUS_DEFAULT_NO) return false;
        }
        return decision;
    }
    
    private int getVisibleParent(int nodeID)
    {
        int parent = treeModel.getParent(nodeID, treeLayer);
        while (!nodeIsVisible(parent))
        {
            parent = treeModel.getParent(parent, treeLayer);
        }
        return parent;
    }
    
    private ArrayList<Integer> getVisibleVirtualChildren(KahinaTree treeModel, int nodeID)
    {
        ArrayList<Integer> descendants = new ArrayList<Integer>();
        //System.err.println("Actual children for node " + nodeID + ": " + treeModel.getChildren(nodeID,treeLayer));
        if (treeModel.isCollapsed(nodeID) && collapsePolicy == COLLAPSE_PRIMARY) return descendants;
        descendants.addAll(treeModel.getChildren(nodeID,treeLayer));
        for (int i = 0; i < descendants.size(); i++)
        {
        	boolean nodeIsVisible = nodeIsVisible(descendants.get(i));
            if (!nodeIsVisible)
            {
            	List<Integer> x = treeModel.getChildren(descendants.remove(i),treeLayer);
                descendants.addAll(x);
                i--;
            }
        }
        //System.err.println("Virtual children for node " + nodeID + ": " + descendants);
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
            sum.start.add(0,1);
            sum.end.add(0,1);
            return sum;
        }
        return new WidthVector();
    }
    
    private WidthVector constructTerminalWidthVector()
    {
        WidthVector widthVector = new WidthVector();
        for (int i = 0; i < nodeLevels.size(); i++)
        {
            widthVector.start.add(1);
            widthVector.end.add(1);
        }
        return widthVector;
    }
    
    public FontMetrics getFontMetrics(Font f, Stroke s, int fontSize)
    {
        //hack to allow precalculations from outside any drawing method
        if ( g == null)
        {
            BufferedImage bufferedImage = new BufferedImage(2,2,BufferedImage.TYPE_4BYTE_ABGR_PRE);
            g =  (Graphics2D) bufferedImage.createGraphics();
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
    
    public void swapDimensions()
    {
        if (secondaryTreeModel != null)
        {
        	dimensionsSwapped = !dimensionsSwapped;
            KahinaTree firstModel = treeModel;
            treeModel = secondaryTreeModel;
            secondaryTreeModel = firstModel;
            resetAllStructures();
            calculateCoordinates();
        }
    }
    
    //TODO: make this work also with bottom-up display orientation
    public int nodeAtCoordinates(int x, int y)
    {      
        boolean bottomUpVariant = false;
        //change display orientation locally to simplify computations
        if (displayOrientation == BOTTOM_UP_DISPLAY)
        {
            y = totalTreeHeight - y;
            bottomUpVariant = true;
            displayOrientation = TOP_DOWN_DISPLAY;
        }
        //binary search over node levels to determine row
        int lowerIndex = 0;
        int upperIndex = nodeLevels.size() - 1;
        int middleIndex = (lowerIndex + upperIndex)/2;
        int middleBound = nodeY.get(nodeLevels.get(middleIndex).get(0)) + 2;
        if (bottomUpVariant) middleBound += getNodeHeight(nodeLevels.get(middleIndex).get(0));
        if (upperIndex != 0) //simply take the only existing level if there is only one
        {
            while (lowerIndex + 1 < upperIndex)
            {
                //System.err.println("lower: " + lowerIndex + " upper: " + upperIndex + " middle bound: " + middleBound + " y: " + y);
                if (middleBound >= y)
                {
                    upperIndex = middleIndex;
                }
                else
                {
                    lowerIndex = middleIndex;
                }
                middleIndex = (lowerIndex + upperIndex)/2;
                middleBound = nodeY.get(nodeLevels.get(middleIndex).get(0)) + 2;
                if (bottomUpVariant) middleBound += getNodeHeight(nodeLevels.get(middleIndex).get(0));
            }
            if (y < middleBound) upperIndex--;
        }
        //System.err.println("Node Level: " + upperIndex);  
        
        //binary search over nodes in 
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
            middleIndex = (lowerIndex + upperIndex)/2;
            FontMetrics fm = getFontMetrics(new Font(Font.SANS_SERIF,Font.PLAIN, fontSize), new BasicStroke(1), fontSize);
            int width = fm.stringWidth(getContentfulTreeModel().getNodeCaption(selectedLevel.get(middleIndex)));
            middleBound = nodeX.get(selectedLevel.get(middleIndex)) + width / 2 + 2;
            if (nodePositionPolicy == KahinaTreeView.LEFT_ALIGNED_NODES)
            {
                middleBound += width / 2;
            }
            else if (nodePositionPolicy == KahinaTreeView.RIGHT_ALIGNED_NODES)
            {
                middleBound -= width / 2;
            }
            //System.err.println("lower: " + lowerIndex + " upper: " + upperIndex + " middle bound: " + middleBound + " x: " + x);
            while (lowerIndex  + 1 < upperIndex)
            {
                if (middleBound >= x)
                {
                    upperIndex = middleIndex;
                }
                else
                {
                    lowerIndex = middleIndex;
                }
                middleIndex = (lowerIndex + upperIndex)/2;
                width = fm.stringWidth(getContentfulTreeModel().getNodeCaption(selectedLevel.get(middleIndex)));
                middleBound = nodeX.get(selectedLevel.get(middleIndex)) + width / 2 - 2;
                if (nodePositionPolicy == KahinaTreeView.LEFT_ALIGNED_NODES)
                {
                    middleBound += width / 2;
                }
                else if (nodePositionPolicy == KahinaTreeView.RIGHT_ALIGNED_NODES)
                {
                    middleBound -= width / 2;
                }   
                //System.err.println("lower: " + lowerIndex + " upper: " + upperIndex + " middle bound: " + middleBound + " x: " + x);
            }
            if (x < middleBound) upperIndex--;
            candidateNode = selectedLevel.get(upperIndex);
        }
        //System.err.println("Potentially clicked node: " + candidateNode);      
            
        //test coordinates against exact boundaries of candidate node
        FontMetrics fm = getFontMetrics(new Font(Font.SANS_SERIF,Font.PLAIN, fontSize), new BasicStroke(1), fontSize);
        int width = fm.stringWidth(getContentfulTreeModel().getNodeCaption(candidateNode));
        int xLeft = getNodeX(candidateNode) - width / 2 - 2;
        if (nodePositionPolicy == KahinaTreeView.LEFT_ALIGNED_NODES)
        {
            xLeft += width / 2;
        }
        else if (nodePositionPolicy == KahinaTreeView.RIGHT_ALIGNED_NODES)
        {
            xLeft -= width / 2;
        }
        int xRight = xLeft + width + 4;   
        int yBottom = getNodeY(candidateNode) + 2;
        int yTop = yBottom - getNodeHeight(candidateNode);
        if (bottomUpVariant)
        {
            yTop = yBottom - 4;
            yBottom = yTop + getNodeHeight(candidateNode);
        }
        //calculations with mirrored tree completed; return to correct mode for rendering
        if (bottomUpVariant)
        {
            displayOrientation = BOTTOM_UP_DISPLAY;
        }
        //System.err.println("test: " + xLeft + " <= " + x + " <= " + xRight + "; " + yTop + " <= " + y + " <= " + yBottom);
        if (xLeft <= x && x <= xRight && yTop <= y && y <= yBottom)
        {
            //System.err.println("Click on node: " + candidateNode);
        }
        else
        {
            //System.err.println("No node found!");
            return -1;
        }
        return candidateNode;
    }

    public KahinaTree getTreeModel()
    {
        return treeModel;
    }
    
    public KahinaTree getContentfulTreeModel()
    {
    	if (dimensionsSwapped)
    	{
    		return secondaryTreeModel;
    	} else
    	{
    		return treeModel;
    	}
    }
}
