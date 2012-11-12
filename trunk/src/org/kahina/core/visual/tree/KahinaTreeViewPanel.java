package org.kahina.core.visual.tree;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.util.List;

import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import org.kahina.core.KahinaInstance;
import org.kahina.core.util.SwingUtil;
import org.kahina.core.visual.KahinaViewPanel;

public class KahinaTreeViewPanel extends KahinaViewPanel<KahinaTreeView>
{
	private static final long serialVersionUID = 6701252380309408342L;
	
	protected BufferedImage image;
    
    public KahinaTreeViewPanel(KahinaInstance<?, ?, ?, ?> kahina)
    {       
        view = new KahinaTreeView(kahina);
        image = new BufferedImage(5, 5, BufferedImage.TYPE_4BYTE_ABGR);
        this.addMouseListener(new KahinaTreeViewListener(this, kahina));
    }
    
    public KahinaTreeViewPanel(KahinaTreeViewMarker marker, KahinaInstance<?, ?, ?, ?> kahina)
    {       
        view = new KahinaTreeView(kahina);
        image = new BufferedImage(5, 5, BufferedImage.TYPE_4BYTE_ABGR);
        this.addMouseListener(new KahinaTreeViewListener(this, marker, kahina));
    }
    
    @Override
	public void paintComponent(Graphics cnv)
    {
        try
        {
            Thread.sleep(10);
            super.paintComponent(cnv);
            if ( image == null ) 
            {
                return;
            }
            cnv.drawImage(image, 0, 0, this );
        }
        catch (InterruptedException e)
        {
            System.err.println("Sleep interrupted!");
        }    
    }
    
    @Override
	public void updateDisplay()
    {     
        BufferedImage newImage = new BufferedImage(view.getDisplayWidth() + 1, view.getDisplayHeight() + 1, BufferedImage.TYPE_4BYTE_ABGR);
        Graphics cnv = newImage.getGraphics();
        Graphics2D canvas = (Graphics2D) cnv;
        if (view.getConfig().getAntialiasingPolicy() == KahinaTreeViewOptions.ANTIALIASING)
        {
            canvas.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        }
        //determine font size
        int fontSize = view.getConfig().getNodeSize();
        Font font = new Font("Arial", Font.PLAIN, fontSize);
        canvas.setFont(font);

        clearCanvas(canvas);
        /*for (TreeViewExtension ext : viewExtensionsBeforeMainRendering)
        {
            ext.paintOnTreePanel(this, canvas);
        }*/
        canvas.setStroke(new BasicStroke(1));
        canvas.setColor(Color.BLACK);
        if (view.getConfig().getTerminalsPolicy() == KahinaTreeViewOptions.GRAPHICALLY_SEPARATED)
        {
            int y = view.getDisplayHeight() - 2 * view.getConfig().getNodeSize() * view.getConfig().getVerticalDistance();
            int y2 = view.getDisplayHeight();
            canvas.drawLine(0, y, view.getDisplayWidth() - 1, y);
            canvas.drawLine(3, y+3, view.getDisplayWidth() - 4, y+3);
            canvas.drawLine(0, y, 0, y2);
            canvas.drawLine(3, y+3, 3, y2 - 3);
            canvas.drawLine(view.getDisplayWidth() - 1, y, view.getDisplayWidth() - 1, y2);
            canvas.drawLine(view.getDisplayWidth() - 4, y+3, view.getDisplayWidth() - 4, y2 - 3);
            canvas.drawLine(3, y2 - 4, view.getDisplayWidth() - 4, y2 - 4);
            canvas.drawLine(0, y2 - 1, view.getDisplayWidth() - 1, y2 - 1);
        }
        printTreeEdges(canvas);
        printSecondaryTreeEdges(canvas);
        printTreeNodes(canvas);
        
        image = newImage;
        
        //TODO: improve behavior when View is smaller than viewport (e.g. add dummy material)
        if (view.getConfig().getAutoscrollPolicy() == KahinaTreeViewOptions.AUTOSCROLL_TO_MARKED_NODE)
        {
            scrollToNode(view.getMarkedNode());
        }
        revalidate();
        repaint();
        
        /*for (TreeViewExtension ext : viewExtensionsAfterMainRendering)
        {
            ext.paintOnTreePanel(this, canvas);
        }*/
    }
    
    public void clearCanvas(Graphics canvas)
    {
        // clear canvas
        Dimension newD = new Dimension(view.getDisplayWidth(), view.getDisplayHeight());
        this.setSize(newD);
        this.setMinimumSize(newD);
        this.setMaximumSize(newD);
        this.setPreferredSize(newD);
        this.setBackground(view.getConfig().getBackgroundColor());
        canvas.setColor(view.getConfig().getBackgroundColor());
        //little hack to account for small trees
        canvas.fillRect(0, 0, 2000, 2000);
        canvas.fillRect(0, 0, this.getSize().width, this.getSize().height);
    }
    
    public void printTreeNodes(Graphics2D cnv)
    {
        //TODO: make this dependent on current scrolling window, ONLY draw nodes there (redraw happens anyway!)
        //print nodes of the tree
        for (int i = 0; i < view.nodeLevels.size(); i++)
        {
            List<Integer> nodes = view.nodeLevels.get(i);
            for (int j = 0; j < nodes.size(); j++)
            {
                if (view.getConfig().getNodeShapePolicy() == KahinaTreeViewOptions.NODE_SHAPE_POINT)
                {
                    printNodePoint(cnv, nodes.get(j));
                }
                else if (view.getConfig().getNodeShapePolicy() == KahinaTreeViewOptions.NODE_SHAPE_BOX)
                {
                    printBoxAroundNodeTag(cnv, nodes.get(j));    
                    printNodeTag(cnv, nodes.get(j));
                }
                else if (view.getConfig().getNodeShapePolicy() == KahinaTreeViewOptions.NODE_SHAPE_OVAL)
                {
                    printOvalAroundNodeTag(cnv, nodes.get(j));
                    printNodeTag(cnv, nodes.get(j));
                }
            }
        }
    }
    
    public void printBoxAroundNodeTag(Graphics2D canvas, int nodeID)
    {
        canvas.setFont(view.getNodeFont(nodeID));
        FontMetrics fm = canvas.getFontMetrics();
        int width = fm.stringWidth(view.getContentfulTreeModel().getNodeCaption(nodeID));
        int x = view.getNodeX(nodeID) - width / 2;
        int y = view.getNodeY(nodeID) - view.getNodeHeight(nodeID) + 2;
        if (view.getConfig().getNodePositionPolicy() == KahinaTreeViewOptions.LEFT_ALIGNED_NODES)
        {
            x += width / 2;
        }
        else if (view.getConfig().getNodePositionPolicy() == KahinaTreeViewOptions.RIGHT_ALIGNED_NODES)
        {
            x -= width / 2;
        }
        Color color = view.getNodeColor(nodeID);
        if (color != null)
        { 
            canvas.setColor(color);
            canvas.fillRect(x - 2, y, width + 4, view.getNodeHeight(nodeID) + 2);
            canvas.setColor(Color.BLACK);
            canvas.drawRect(x - 2, y, width + 4, view.getNodeHeight(nodeID) + 2);
            if (view.getMarkedNode() == nodeID)
            {
                canvas.setColor(Color.YELLOW);
                canvas.setStroke(new BasicStroke(2));
                canvas.drawRect(x - 3, y - 1, width + 6, view.getNodeHeight(nodeID) + 4);
            }
            if (view.getNodeBorderColor(nodeID) != null)
            {
                canvas.setColor(view.getNodeBorderColor(nodeID));
                canvas.setStroke(new BasicStroke(2));
                canvas.drawRect(x - 3, y - 1, width + 6, view.getNodeHeight(nodeID) + 4);
            }
        }
    }
    
    public void printOvalAroundNodeTag(Graphics2D canvas, int nodeID)
    {
        canvas.setFont(view.getNodeFont(nodeID));
        FontMetrics fm = canvas.getFontMetrics();
        int width = fm.stringWidth(view.getContentfulTreeModel().getNodeCaption(nodeID));
        int x = view.getNodeX(nodeID) - width / 2;
        int y = view.getNodeY(nodeID) - view.getNodeHeight(nodeID) + 2;
        //TODO: correct oval drawing also for non-central alignments
        if (view.getConfig().getNodePositionPolicy() == KahinaTreeViewOptions.LEFT_ALIGNED_NODES)
        {
            x += width / 2;
        }
        else if (view.getConfig().getNodePositionPolicy() == KahinaTreeViewOptions.RIGHT_ALIGNED_NODES)
        {
            x -= width / 2;
        }
        Color color = view.getNodeColor(nodeID);
        if (color != null)
        { 
            canvas.setColor(color);
            canvas.fillOval(x - 2, y, width + 4, view.getNodeHeight(nodeID) + 4);
            canvas.setColor(Color.BLACK);
            canvas.drawOval(x - 2, y, width + 4, view.getNodeHeight(nodeID) + 4);
            if (view.getMarkedNode() == nodeID)
            {
                canvas.setColor(new Color(255,255,170));
                canvas.setStroke(new BasicStroke(2));
                canvas.drawOval(x - 3, y - 1, width + 6, view.getNodeHeight(nodeID) + 6);
            }
        }
    }
    
    public void printNodeTag(Graphics2D canvas, int nodeID)
    {
        canvas.setFont(view.getNodeFont(nodeID));
        FontMetrics fm = canvas.getFontMetrics();
        String tag = view.getContentfulTreeModel().getNodeCaption(nodeID);
        int width = fm.stringWidth(tag);
        canvas.setColor(Color.BLACK);
        //print tag name of node
        int x = view.getNodeX(nodeID) - width / 2;
        if (view.getConfig().getNodePositionPolicy() == KahinaTreeViewOptions.LEFT_ALIGNED_NODES)
        {
            x += width / 2;
        }
        else if (view.getConfig().getNodePositionPolicy() == KahinaTreeViewOptions.RIGHT_ALIGNED_NODES)
        {
            x -= width / 2;
        }
        int y = view.getNodeY(nodeID);
        canvas.drawString(tag, x, y);
        //canvas.drawString(tag + " " + nodeID, x, y);
        canvas.setStroke(new BasicStroke(1));
        canvas.setFont(new Font(canvas.getFont().getFontName(),Font.PLAIN, view.getConfig().getNodeSize()));
    }
    
    public void printNodePoint(Graphics2D canvas, int vertex)
    {
        int size = view.getConfig().getNodeSize();
        int x = view.getNodeX(vertex) - size / 2;
        int y = view.getNodeY(vertex) - size / 2;
        Color color = view.getNodeColor(vertex);
        if (color != null)
        { 
            canvas.setColor(color);
            canvas.fillOval(x, y, size, size);
            Color nodeBorderColor = view.getNodeBorderColor(vertex);
            if (nodeBorderColor == null || nodeBorderColor == Color.WHITE)
            {
                nodeBorderColor = Color.black;
            }
            canvas.setColor(nodeBorderColor);
            canvas.drawOval(x - 1, y - 1, size + 2, size + 2);
        }
    }
    
    public void printTreeEdges(Graphics canvas)
    {
        // create lines and their tags
        canvas.setColor(Color.BLACK);
        for (int i = 0; i < view.nodeLevels.size(); i++)
        {
            List<Integer> nodes = view.nodeLevels.get(i);
            for (int nodeID : nodes)
            {
                if (view.getModel().getParent(nodeID,view.getTreeLayer()) != -1)
                {
                    int parent = view.getModel().getParent(nodeID,view.getTreeLayer());
                    while (!view.displaysNode(parent))
                    {
                        parent = view.getModel().getParent(parent,view.getTreeLayer());
                    }
                    int x1 = view.getNodeX(parent);
                    int y1 = view.getNodeY(parent);
                    int x2 = view.getNodeX(nodeID);
                    int y2 = view.getNodeY(nodeID);
                    if (view.getConfig().getDisplayOrientation() == KahinaTreeViewOptions.TOP_DOWN_DISPLAY)
                    {
                        y1 += view.getNodeHeight(view.getModel().getParent(nodeID,view.getTreeLayer())) - view.getConfig().getNodeSize();
                        y2 -= view.getConfig().getNodeSize() - 2;
                    }
                    String edgeTag = view.getContentfulTreeModel().getEdgeLabel(nodeID);
                    switch (view.getConfig().getLineShapePolicy())
                    {
                        case KahinaTreeViewOptions.STRAIGHT_LINES:
                        {
                            drawLineAccordingToType(canvas,view.getEdgeStyle(nodeID),x1, y1, x2, y2);
                            printEdgeTag(canvas, new Point((x2+x1)/2,(y2+y1)/2),edgeTag);
                            break;
                        }
                        case KahinaTreeViewOptions.EDGY_LINES:
                        {
                            drawLineAccordingToType(canvas,view.getEdgeStyle(nodeID),x1, y1, x2, y1);
                            drawLineAccordingToType(canvas,view.getEdgeStyle(nodeID),x2, y1, x2, y2);
                            printEdgeTag(canvas, new Point(x2,(y2+y1)/2),edgeTag);
                            break;
                        }
                        //third case: do not draw anything
                    }
                    //TODO: add this later
                    //printEdgeArrow(canvas, nodes.get(j));             
                }
            }
        }
    }
    
    public void printEdgeTag(Graphics canvas, Point center, String tag)
    {
        if (tag != null && view.getConfig().getEdgeTagPolicy() != KahinaTreeViewOptions.NO_EDGE_TAGS)
        {
            FontMetrics fm = canvas.getFontMetrics();
            int width = fm.stringWidth(tag);
            int height = fm.getHeight();
            canvas.setColor(Color.BLACK);
            //print edge tag
            int x = center.x - width / 2;
            int y = center.y;
            if (view.getConfig().getEdgeTagPolicy() == KahinaTreeViewOptions.OVAL_EDGE_TAGS)
            {
                canvas.setColor(Color.WHITE);
                canvas.fillOval(x - 2, y - height + 2, width + 4, height + 2);
                canvas.setColor(Color.BLACK);
                canvas.drawOval(x - 2, y - height + 2, width + 4, height + 2);
            }
            else if (view.getConfig().getEdgeTagPolicy() == KahinaTreeViewOptions.BOXED_EDGE_TAGS)
            {
                canvas.setColor(Color.WHITE);
                canvas.fillRect(x - 2, y - height + 2, width + 4, height + 2);
                canvas.setColor(Color.BLACK);
                canvas.drawRect(x - 2, y - height + 2, width + 4, height + 2);
            }
            canvas.drawString(tag, x, y);
        }
    }
    
    public void printSecondaryTreeEdges(Graphics canvas)
    {
        if (view.secondaryTreeModel != null)
        {
            canvas.setColor(Color.GRAY);
            for (int i = 0; i < view.nodeLevels.size(); i++)
            {
                List<Integer> nodes = view.nodeLevels.get(i);
                for (int node : nodes)
                {
                    printSecondaryTreeEdgeForNode(node, canvas);
                } 
            }
            //print edge for marked node last, making sure it is highlighted well
            canvas.setColor(Color.BLACK);
            ((Graphics2D) canvas).setStroke(new BasicStroke(2));
            printSecondaryTreeEdgeForNode(view.getMarkedNode(), canvas);
        }
    }
    
    protected void printSecondaryTreeEdgeForNode(int node, Graphics canvas)
    {
    	int parent = view.secondaryTreeModel.getParent(node, view.treeLayer);
        if (parent != -1 && view.displaysNode(parent))
        {
            switch (view.getConfig().getSecondaryLineShapePolicy())
            {
                case KahinaTreeViewOptions.EDGY_LINES:
                {
                    int x1 = view.getNodeX(parent);
                    int x2 = view.getNodeX(node);
                    int y1 = view.getNodeY(parent);
                    int y2 = view.getNodeY(node);
                    int zoomLevel = view.getConfig().getNodeSize();
                    if (view.getConfig().getDisplayOrientation() == KahinaTreeViewOptions.BOTTOM_UP_DISPLAY)
                    {
                        y2 += zoomLevel / 2;
                        y1 -= zoomLevel / 2;
                    }
                    if (view.getConfig().getNodePositionPolicy() == KahinaTreeViewOptions.LEFT_ALIGNED_NODES)
                    {
                        int x3 = x1;
                        if (x2 <= x1) x3 = x2;
                        canvas.drawLine(x3 - zoomLevel, y1, x1, y1);
                        canvas.drawLine(x3 - zoomLevel, y1, x3 - zoomLevel, y2 - zoomLevel / 2);
                        canvas.drawLine(x3 - zoomLevel, y2 - zoomLevel / 2, x2, y2 - zoomLevel / 2);
                    }
                    else if (view.getConfig().getNodePositionPolicy() == KahinaTreeViewOptions.CENTERED_NODES)
                    {
                        int parentWidth = canvas.getFontMetrics().stringWidth(view.getContentfulTreeModel().getNodeCaption(parent));
                        int nodeWidth = canvas.getFontMetrics().stringWidth(view.getContentfulTreeModel().getNodeCaption(node));
                        x1 -= parentWidth / 2;
                        x2 -= nodeWidth / 2;
                        int x3 = x1;
                        if (x2 <= x1) x3 = x2;
                        canvas.drawLine(x3 - zoomLevel, y1, x1, y1);
                        canvas.drawLine(x3 - zoomLevel, y1, x3 - zoomLevel, y2 - zoomLevel / 2);
                        canvas.drawLine(x3 - zoomLevel, y2 - zoomLevel / 2, x2, y2 - zoomLevel / 2);
                    }
                    else if (view.getConfig().getNodePositionPolicy() == KahinaTreeViewOptions.RIGHT_ALIGNED_NODES)
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
    
    public static void drawLineAccordingToType(Graphics g, int type, int x0, int y0, int x1, int y1)
    {
        if (type == KahinaTreeViewOptions.DOTTED_LINES)
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
    
    public void scrollToNode(final int nodeID)
    {
        //System.err.println("Scrolling to node " + nodeID + " on layer " + view.treeLayer);
        if (this.getParent() == null) return;
        final Container parent = this.getParent().getParent();
        if (nodeID != -1 && parent instanceof JScrollPane)
        {
            Integer x = view.getNodeX(nodeID);
            Integer y = view.getNodeY(nodeID);
            if (x == null || y == null)
            {
                System.err.println("KahinaTreeView: Could not find scroll coordinates for node " + nodeID + "!");
            }
            else
            {
            	SwingUtilities.invokeLater(new Runnable() {

					@Override
					public void run() {
		                SwingUtil.scrollToCenter((JScrollPane) parent, view.getNodeX(nodeID), view.getNodeY(nodeID));
					}});
            }
        }
    }
}
