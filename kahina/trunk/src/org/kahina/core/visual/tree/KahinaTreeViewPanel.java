package org.kahina.core.visual.tree;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.util.List;

import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.visual.KahinaDefaultView;
import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.KahinaViewPanel;

public class KahinaTreeViewPanel extends KahinaViewPanel<KahinaTreeView>
{
    protected KahinaTreeView v;
    BufferedImage image;
    
    public KahinaTreeViewPanel()
    {       
        v = new KahinaTreeView();
        image = new BufferedImage(5, 5, BufferedImage.TYPE_4BYTE_ABGR);
        this.addMouseListener(new KahinaTreeViewListener(this));
    }
    
    public KahinaTreeViewPanel(KahinaTreeViewMarker marker)
    {       
        v = new KahinaTreeView();
        image = new BufferedImage(5, 5, BufferedImage.TYPE_4BYTE_ABGR);
        this.addMouseListener(new KahinaTreeViewListener(this, marker));
    }
    
    public void setView(KahinaTreeView view)
    {
        this.v = view;
        updateDisplay();
        repaint();
    }
    
    public void paintComponent(Graphics cnv)
    {
        try
        {
            Thread.sleep(10);
            if ( image == null ) 
            {
                return;
            }
            cnv.drawImage( image, 0, 0, this );
        }
        catch (InterruptedException e)
        {
            System.err.println("Sleep interrupted!");
        }    
    }
    
    public void updateDisplay()
    {      
        BufferedImage newImage = new BufferedImage(v.getDisplayWidth() + 1, v.getDisplayHeight() + 1, BufferedImage.TYPE_4BYTE_ABGR);
        Graphics cnv = newImage.getGraphics();
        Graphics2D canvas = (Graphics2D) cnv;
        if (v.getAntialiasingPolicy() == KahinaTreeView.ANTIALIASING)
        {
            canvas.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        }
        //determine font size
        int fontSize = v.getZoomLevel();
        Font font = new Font("Arial", Font.PLAIN, fontSize);
        canvas.setFont(font);

        clearCanvas(canvas);
        /*for (TreeViewExtension ext : viewExtensionsBeforeMainRendering)
        {
            ext.paintOnTreePanel(this, canvas);
        }*/
        canvas.setStroke(new BasicStroke(1));
        canvas.setColor(Color.BLACK);
        if (v.getTerminalsPolicy() == KahinaTreeView.GRAPHICALLY_SEPARATED)
        {
            int y = v.getDisplayHeight() - 2 * v.getZoomLevel() * v.getVerticalDistance();
            int y2 = v.getDisplayHeight();
            canvas.drawLine(0, y, v.getDisplayWidth() - 1, y);
            canvas.drawLine(3, y+3, v.getDisplayWidth() - 4, y+3);
            canvas.drawLine(0, y, 0, y2);
            canvas.drawLine(3, y+3, 3, y2 - 3);
            canvas.drawLine(v.getDisplayWidth() - 1, y, v.getDisplayWidth() - 1, y2);
            canvas.drawLine(v.getDisplayWidth() - 4, y+3, v.getDisplayWidth() - 4, y2 - 3);
            canvas.drawLine(3, y2 - 4, v.getDisplayWidth() - 4, y2 - 4);
            canvas.drawLine(0, y2 - 1, v.getDisplayWidth() - 1, y2 - 1);
        }
        printTreeEdges(canvas);
        printSecondaryTreeEdges(canvas);
        printTreeNodes(canvas);
        
        image = newImage;

        /*for (TreeViewExtension ext : viewExtensionsAfterMainRendering)
        {
            ext.paintOnTreePanel(this, canvas);
        }*/
    }
    
    public void clearCanvas(Graphics canvas)
    {
        // clear canvas
        Dimension newD = new Dimension(v.getDisplayWidth(), v.getDisplayHeight());
        this.setSize(newD);
        this.setMinimumSize(newD);
        this.setMaximumSize(newD);
        this.setPreferredSize(newD);
        this.setBackground(v.bgColor);
        canvas.setColor(v.bgColor);
        //little hack to account for small trees
        canvas.fillRect(0, 0, 2000, 2000);
        canvas.fillRect(0, 0, this.getSize().width, this.getSize().height);
    }
    
    public void printTreeNodes(Graphics2D cnv)
    {
        //TODO: make this dependent on current scrolling window, ONLY draw nodes there (redraw happens anyway!)
        //print nodes of the tree
        for (int i = 0; i < v.nodeLevels.size(); i++)
        {
            List<Integer> nodes = v.nodeLevels.get(i);
            for (int j = 0; j < nodes.size(); j++)
            {
                if (v.getNodeShapePolicy() == KahinaTreeView.BOX_SHAPE)
                {
                    printBoxAroundNodeTag(cnv, nodes.get(j));               
                }
                else if (v.getNodeShapePolicy() == KahinaTreeView.OVAL_SHAPE)
                {
                    printOvalAroundNodeTag(cnv, nodes.get(j));
                }
                printNodeTag(cnv, nodes.get(j));
            }
        }
    }
    
    public void printBoxAroundNodeTag(Graphics2D canvas, int nodeID)
    {
        canvas.setFont(v.getNodeFont(nodeID));
        FontMetrics fm = canvas.getFontMetrics();
        int width = fm.stringWidth(v.getContentfulTreeModel().getNodeCaption(nodeID));
        int x = v.getNodeX(nodeID) - width / 2;
        int y = v.getNodeY(nodeID) - v.getNodeHeight(nodeID) + 2;
        if (v.getNodePositionPolicy() == KahinaTreeView.LEFT_ALIGNED_NODES)
        {
            x += width / 2;
        }
        else if (v.getNodePositionPolicy() == KahinaTreeView.RIGHT_ALIGNED_NODES)
        {
            x -= width / 2;
        }
        Color color = v.getNodeColor(nodeID);
        if (color != null)
        { 
            canvas.setColor(color);
            canvas.fillRect(x - 2, y, width + 4, v.getNodeHeight(nodeID) + 2);
            canvas.setColor(Color.BLACK);
            canvas.drawRect(x - 2, y, width + 4, v.getNodeHeight(nodeID) + 2);
            if (v.getMarkedNode() == nodeID)
            {
                canvas.setColor(Color.ORANGE);
                canvas.setStroke(new BasicStroke(2));
                canvas.drawRect(x - 3, y - 1, width + 6, v.getNodeHeight(nodeID) + 4);
            }
            if (v.getNodeBorderColor(nodeID) != null)
            {
                canvas.setColor(v.getNodeBorderColor(nodeID));
                canvas.setStroke(new BasicStroke(2));
                canvas.drawRect(x - 3, y - 1, width + 6, v.getNodeHeight(nodeID) + 4);
            }
        }
    }
    
    public void printOvalAroundNodeTag(Graphics2D canvas, int nodeID)
    {
        canvas.setFont(v.getNodeFont(nodeID));
        FontMetrics fm = canvas.getFontMetrics();
        int width = fm.stringWidth(v.getContentfulTreeModel().getNodeCaption(nodeID));
        int x = v.getNodeX(nodeID) - width / 2;
        int y = v.getNodeY(nodeID) - v.getNodeHeight(nodeID) + 2;
        //TODO: correct oval drawing also for non-central alignments
        if (v.getNodePositionPolicy() == KahinaTreeView.LEFT_ALIGNED_NODES)
        {
            x += width / 2;
        }
        else if (v.getNodePositionPolicy() == KahinaTreeView.RIGHT_ALIGNED_NODES)
        {
            x -= width / 2;
        }
        Color color = v.getNodeColor(nodeID);
        if (color != null)
        { 
            canvas.setColor(color);
            canvas.fillOval(x - 2, y, width + 4, v.getNodeHeight(nodeID) + 4);
            canvas.setColor(Color.BLACK);
            canvas.drawOval(x - 2, y, width + 4, v.getNodeHeight(nodeID) + 4);
            if (v.getMarkedNode() == nodeID)
            {
                canvas.setColor(Color.ORANGE);
                canvas.setStroke(new BasicStroke(2));
                canvas.drawOval(x - 3, y - 1, width + 6, v.getNodeHeight(nodeID) + 6);
            }
        }
    }
    
    public void printNodeTag(Graphics2D canvas, int nodeID)
    {
        canvas.setFont(v.getNodeFont(nodeID));
        FontMetrics fm = canvas.getFontMetrics();
        String tag = v.getContentfulTreeModel().getNodeCaption(nodeID);
        int width = fm.stringWidth(tag);
        canvas.setColor(Color.BLACK);
        //print tag name of node
        int x = v.getNodeX(nodeID) - width / 2;
        if (v.getNodePositionPolicy() == KahinaTreeView.LEFT_ALIGNED_NODES)
        {
            x += width / 2;
        }
        else if (v.getNodePositionPolicy() == KahinaTreeView.RIGHT_ALIGNED_NODES)
        {
            x -= width / 2;
        }
        int y = v.getNodeY(nodeID);  
        canvas.drawString(tag, x, y);
        canvas.setStroke(new BasicStroke(1));
        canvas.setFont(new Font(canvas.getFont().getFontName(),Font.PLAIN, v.getZoomLevel()));
    }
    
    public void printTreeEdges(Graphics canvas)
    {
        // create lines and their tags
        canvas.setColor(Color.BLACK);
        for (int i = 0; i < v.nodeLevels.size(); i++)
        {
            List<Integer> nodes = v.nodeLevels.get(i);
            for (int nodeID : nodes)
            {
                if (v.getModel().getParent(nodeID,v.getTreeLayer()) != -1)
                {
                    int parent = v.getModel().getParent(nodeID,v.getTreeLayer());
                    while (!v.displaysNode(parent))
                    {
                        parent = v.getModel().getParent(parent,v.getTreeLayer());
                    }
                    int x1 = v.getNodeX(parent);
                    int y1 = v.getNodeY(parent);
                    int x2 = v.getNodeX(nodeID);
                    int y2 = v.getNodeY(nodeID);
                    if (v.getDisplayOrientation() == KahinaTreeView.TOP_DOWN_DISPLAY)
                    {
                        y1 += v.getNodeHeight(v.getModel().getParent(nodeID,v.getTreeLayer())) + 4 - v.getZoomLevel();
                        y2 -= v.getZoomLevel() - 2;
                    }
                    switch (v.getLineShapePolicy())
                    {
                        case KahinaTreeView.STRAIGHT_LINES:
                        {
                            drawLineAccordingToType(canvas,v.getEdgeStyle(nodeID),x1, y1, x2, y2);
                            break;
                        }
                        case KahinaTreeView.EDGY_LINES:
                        {
                            drawLineAccordingToType(canvas,v.getEdgeStyle(nodeID),x1, y1, x2, y1);
                            drawLineAccordingToType(canvas,v.getEdgeStyle(nodeID),x2, y1, x2, y2);
                            break;
                        }
                        //third case: do not draw anything
                    }
                    //TODO: add this later
                    //printEdgeTag(canvas,nodes.get(j),edgyLines);
                    //printEdgeArrow(canvas, nodes.get(j));             
                }
            }
        }
    }
    
    public void printSecondaryTreeEdges(Graphics canvas)
    {
        if (v.secondaryTreeModel != null)
        {
            canvas.setColor(Color.GRAY);
            for (int i = 0; i < v.nodeLevels.size(); i++)
            {
                List<Integer> nodes = v.nodeLevels.get(i);
                for (int node : nodes)
                {
                    if (v.getMarkedNode() == node)
                    {
                        canvas.setColor(Color.BLACK);
                    }
                    else
                    {
                        canvas.setColor(Color.GRAY);
                    }
                    int parent = v.secondaryTreeModel.getParent(node, v.treeLayer);
                    if (parent != -1 && v.displaysNode(parent))
                    {
                        switch (v.getSecondaryLineShapePolicy())
                        {
                            case KahinaTreeView.EDGY_LINES:
                            {
                                int x1 = v.getNodeX(parent);
                                int x2 = v.getNodeX(node);
                                int y1 = v.getNodeY(parent);
                                int y2 = v.getNodeY(node);
                                int zoomLevel = v.getZoomLevel();
                                if (v.getDisplayOrientation() == KahinaTreeView.BOTTOM_UP_DISPLAY)
                                {
                                    y2 += zoomLevel / 2;
                                    y1 -= zoomLevel / 2;
                                }
                                if (v.getNodePositionPolicy() == KahinaTreeView.LEFT_ALIGNED_NODES)
                                {
                                    int x3 = x1;
                                    if (x2 <= x1) x3 = x2;
                                    canvas.drawLine(x3 - zoomLevel, y1, x1, y1);
                                    canvas.drawLine(x3 - zoomLevel, y1, x3 - zoomLevel, y2 - zoomLevel / 2);
                                    canvas.drawLine(x3 - zoomLevel, y2 - zoomLevel / 2, x2, y2 - zoomLevel / 2);
                                }
                                else if (v.getNodePositionPolicy() == KahinaTreeView.CENTERED_NODES)
                                {
                                    int parentWidth = canvas.getFontMetrics().stringWidth(v.getContentfulTreeModel().getNodeCaption(parent));
                                    int nodeWidth = canvas.getFontMetrics().stringWidth(v.getContentfulTreeModel().getNodeCaption(node));
                                    x1 -= parentWidth / 2;
                                    x2 -= nodeWidth / 2;
                                    int x3 = x1;
                                    if (x2 <= x1) x3 = x2;
                                    canvas.drawLine(x3 - zoomLevel, y1, x1, y1);
                                    canvas.drawLine(x3 - zoomLevel, y1, x3 - zoomLevel, y2 - zoomLevel / 2);
                                    canvas.drawLine(x3 - zoomLevel, y2 - zoomLevel / 2, x2, y2 - zoomLevel / 2);
                                }
                                else if (v.getNodePositionPolicy() == KahinaTreeView.RIGHT_ALIGNED_NODES)
                                {
                                    int x3 = x1;
                                    if (x2 > x1) x3 = x2;
                                    canvas.drawLine(x3 + zoomLevel, y1, x1, y1);
                                    canvas.drawLine(x3 + zoomLevel, y1, x3 + zoomLevel, y2 - zoomLevel / 2);
                                    canvas.drawLine(x3 + zoomLevel, y2 - zoomLevel / 2, x2, y2 - zoomLevel / 2);
                                }
                                break;
                            }
                        }
                    }
                }
            }
        }
    }
    
    public static void drawLineAccordingToType(Graphics g, int type, int x0, int y0, int x1, int y1)
    {
        if (type == KahinaTreeView.DOTTED_LINES)
        {
            drawDottedLine(g,x0,y0,x1,y1,g.getColor(),1,1);
        }
        else
        {
            g.drawLine(x0,y0,x1,y1);
        }
    }
    
    public static void drawDottedLine(Graphics g, int x0, int y0, int x1, int y1, Color color, int dashLen, int spaceLen)
    {
        Color c = g.getColor();
        g.setColor(color);
        int dx = x1 - x0;
        int dy = y1 - y0;
        float t = 0.5f;

        g.setColor(color);
        g.drawLine(x0, y0, x0, y0);

        int dashCount = 0;
        int spaceCount = 0;
        boolean doPlot = dashLen > 1;

        if (Math.abs(dx) > Math.abs(dy))
        { // slope < 1
            float m = (float) dy / (float) dx; // compute slope
            t += y0;
            dx = (dx < 0) ? -1 : 1;
            m *= dx;
            while (x0 != x1)
            {
                x0 += dx; // step to next x value
                t += m;
                if (doPlot)
                {
                    g.drawLine(x0, (int) t, x0, (int) t);
                    dashCount++;
                    if (dashCount >= dashLen)
                    {
                        dashCount = 0;
                        spaceCount = 0;
                        doPlot = false;
                    }
                }
                else
                {
                    spaceCount++;
                    if (spaceCount >= spaceLen)
                    {
                        spaceCount = 0;
                        dashCount = 0;
                        doPlot = true;
                    }
                }

            }
        }
        else if (dy != 0)
        { // slope >= 1
            float m = (float) dx / (float) dy; // compute slope
            t += x0;
            dy = (dy < 0) ? -1 : 1;
            m *= dy;
            while (y0 != y1)
            {
                y0 += dy; // step to next y value
                t += m;
                if (doPlot)
                {
                    g.drawLine((int) t, y0, (int) t, y0);
                    dashCount++;
                    if (dashCount >= dashLen)
                    {
                        dashCount = 0;
                        spaceCount = 0;
                        doPlot = false;
                    }
                }
                else
                {
                    spaceCount++;
                    if (spaceCount >= spaceLen)
                    {
                        spaceCount = 0;
                        dashCount = 0;
                        doPlot = true;
                    }
                }
            }
        }
        g.setColor(c);
    }
}
