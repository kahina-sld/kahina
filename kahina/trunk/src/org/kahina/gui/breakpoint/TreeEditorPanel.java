package org.kahina.gui.breakpoint;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.RenderingHints;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JPanel;

import org.kahina.visual.tree.WidthVector;

public class TreeEditorPanel extends JPanel
{
    TreeFragmentPanel fragment;
    
    private HashMap<JPanel, Integer> x;
    private HashMap<JPanel, Integer> y;
    private HashMap<JPanel, WidthVector> subtreeWidths;
    
    public TreeEditorPanel(TreeFragmentPanel fragment)
    {
        this.setLayout(null);
        this.fragment = fragment;
        this.x = new HashMap<JPanel, Integer>();
        this.y = new HashMap<JPanel, Integer>();
    }
    
    public void recalculateCoordinates()
    {
        //System.err.println("Recalculating coordinates!");
        this.subtreeWidths = new HashMap<JPanel, WidthVector>();
        y.put(fragment.getRoot(), 100);
        
        List<List<JPanel>> nodeLevels = createNodeLayers();
        for (List<JPanel> nodeLevel : nodeLevels)
        {
            //System.err.println(nodeLevel);
        }
        
        int totalTreeWidth = 50;
        int totalTreeHeight = 50;   
        
        int vertDist = 0;
             
        if (fragment.getRoot() != null)
        {
            for (int i = nodeLevels.size() - 1; i >= 0; i--)
            {
                //System.err.println("Node level: " + i);
                for (JPanel node : nodeLevels.get(i))
                {
                    subtreeWidths.put(node,constructWidthVector(node));
                    int nodeHeight = node.getPreferredSize().height;
                    if (nodeHeight > vertDist) vertDist = nodeHeight;
                }
            }
            //System.err.println("COMPLETE: Calculate subtree widths for layer " + treeLayer);
            //System.err.println("maxNodeWidth = " + maxNodeWidth);
            x.put(fragment.getRoot(), subtreeWidths.get(fragment.getRoot()).maximumLeftDistance());
            for (int i = 0; i < nodeLevels.size(); i++)
            {
                //System.err.println("Node level: " + i);
                List<JPanel> nodes = nodeLevels.get(i);  
                int xOffset = 0;
                if (nodes.size() > 0) xOffset = subtreeWidths.get(nodes.get(0)).maximumLeftDistance();
                //System.err.println("Start with x offset " + xOffset);
                JPanel parent = null;
                WidthVector subtreeWidth  = new WidthVector();
                WidthVector lastSubtreeWidth;
                for (JPanel node : nodes)
                {
                    //System.err.print("  Node:" + node);
                    lastSubtreeWidth = subtreeWidth;
                    subtreeWidth = subtreeWidths.get(node);
                    //System.err.println("Next necessary distance " + WidthVector.computeNecessaryDistance(lastSubtreeWidth, subtreeWidth));
                    xOffset += WidthVector.computeNecessaryDistance(lastSubtreeWidth, subtreeWidth);
                    //switch to children of next parent node --> jump in x offset
                    JPanel newParent = fragment.getParent(node);
                    //System.err.print(" VisParent:" + newParent);
                    if (i > 0 && newParent != parent)
                    {
                        parent = newParent;
                        //System.err.print(" SubtreeWidths:" + subtreeWidths.get(parent));
                        xOffset = x.get(parent) -  subtreeWidths.get(parent).getStart(1)  + node.getPreferredSize().width / 2;
                    }
                    if (i > 0)
                    {
                        x.put(node, xOffset);
                    }
                    y.put(node, vertDist * i);
                    //System.err.println(" X:" + nodeX.get(node) + " Y:" + nodeY.get(node));
                }
                //adapt total tree width to maximum level width (i.e. maximum x position of a node in any level)     
                if (nodes.size() > 0)
                {
                    int nodeLevelWidth =  x.get(nodes.get(nodes.size() - 1)) + nodes.get(nodes.size() - 1).getPreferredSize().width;
                    if (nodeLevelWidth > totalTreeWidth)
                    {
                       totalTreeWidth = nodeLevelWidth;
                    }
                }
            }
            totalTreeHeight = vertDist * nodeLevels.size();
        }
        for (int i = nodeLevels.size() - 1; i >= 0; i--)
        {
            //System.err.println("Adapting node level: " + i);
            for (JPanel node : nodeLevels.get(i))
            {
                adaptNodeSizeAndPosition(node);
            }
        }
        this.setPreferredSize(new Dimension(totalTreeWidth,totalTreeHeight));
    }
        
    private List<List<JPanel>> createNodeLayers()
    {
      List<List<JPanel>> nodeLevels = new ArrayList<List<JPanel>>();
      JPanel root = fragment.getRoot();
      //System.err.println("creating node layer starting form root " + root);
      ArrayList<JPanel> rootLevel = new ArrayList<JPanel>();
      if (root != null)
      {
          rootLevel.add(root);
          nodeLevels.add(rootLevel);
          List<JPanel> children = new ArrayList<JPanel>();
          children.addAll(fragment.getChildren(root));   
          while(true)
          {
            ArrayList<JPanel> grandchildren = new ArrayList<JPanel>();
            for (int i = 0; i < children.size(); i++)
            {
                grandchildren.addAll(fragment.getChildren(children.get(i)));
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
      return nodeLevels;
    }
    
    private WidthVector constructWidthVector(JPanel node)
    {
        List<JPanel> children = new ArrayList<JPanel>();
        children.addAll(fragment.getChildren(node));  
        int width = node.getPreferredSize().width / 2;
        if (children.size() > 0)
        {
            WidthVector sum = subtreeWidths.get(children.get(0)).copy();
            for (int i = 1; i < children.size(); i++)
            {
                sum = WidthVector.adjoin(sum, subtreeWidths.get(children.get(i)));
            }
            sum.start.add(0,width);
            sum.end.add(0,width);
            //System.err.println(sum);
            return sum;
        }
        return new WidthVector(width,width);
    }
    
    public void adaptNodeSizeAndPosition(JPanel node)
    {
        Insets insets = getInsets();
        Dimension size = node.getPreferredSize();
        node.setBounds(x.get(node) + insets.left , y.get(node) + insets.top, size.width, size.height);
    }
    
    public void paint(Graphics g)
    {
        Insets insets = getInsets();
        super.paint(g);
        Graphics2D canvas = (Graphics2D) g;
        canvas.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        for (JPanel panel : x.keySet())
        {
            JPanel parent = fragment.getParent(panel);
            if (parent != null)
            {
                int bottomX = x.get(panel) + insets.left + parent.getPreferredSize().width / 2;
                int bottomY = y.get(panel) + insets.top + 12;
                int topX = x.get(parent) + insets.left + parent.getPreferredSize().width / 2;
                int topY = y.get(parent) + insets.top + parent.getPreferredSize().height - 8;
                canvas.drawLine(bottomX, bottomY, topX, topY);
            }
        }
    }
    
    public String showLevels()
    {
        String levelString = "";

        return levelString;
    }
}
