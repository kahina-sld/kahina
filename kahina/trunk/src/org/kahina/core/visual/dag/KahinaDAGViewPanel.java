package org.kahina.core.visual.dag;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.util.List;

import javax.swing.JScrollPane;

import org.kahina.core.util.SwingUtilities;
import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.core.visual.tree.KahinaTreeView;
import org.kahina.core.visual.tree.KahinaTreeViewListener;
import org.kahina.core.visual.tree.KahinaTreeViewMarker;

public class KahinaDAGViewPanel extends KahinaViewPanel<KahinaDAGView>
{
private static final long serialVersionUID = 6701252380309408342L;
    
    BufferedImage image;
    
    public KahinaDAGViewPanel()
    {       
        view = new KahinaDAGView();
        image = new BufferedImage(5, 5, BufferedImage.TYPE_4BYTE_ABGR);
        this.addMouseListener(new KahinaDAGViewListener(this));
    }
    
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
    
    public void updateDisplay()
    {      
        BufferedImage newImage = new BufferedImage(view.getDisplayWidth() + 1, view.getDisplayHeight() + 1, BufferedImage.TYPE_4BYTE_ABGR);
        Graphics cnv = newImage.getGraphics();
        Graphics2D canvas = (Graphics2D) cnv;
        if (view.getAntialiasingPolicy() == KahinaTreeView.ANTIALIASING)
        {
            canvas.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        }
        //determine font size
        int fontSize = view.getZoomLevel();
        Font font = new Font("Arial", Font.PLAIN, fontSize);
        canvas.setFont(font);

        clearCanvas(canvas);
        /*for (TreeViewExtension ext : viewExtensionsBeforeMainRendering)
        {
            ext.paintOnTreePanel(this, canvas);
        }*/
        canvas.setStroke(new BasicStroke(1));
        canvas.setColor(Color.BLACK);
        printDAGEdges(canvas);
        printDAGNodes(canvas);
        
        image = newImage;
        repaint();
        
        //TODO: improve behavior when View is smaller than viewport (e.g. add dummy material)
        scrollToNode(view.getMarkedNode());
        revalidate();
        
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
        this.setBackground(view.bgColor);
        canvas.setColor(view.bgColor);
        //little hack to account for small trees
        canvas.fillRect(0, 0, 2000, 2000);
        canvas.fillRect(0, 0, this.getSize().width, this.getSize().height);
    }
    
    public void printDAGNodes(Graphics2D cnv)
    {
        //TODO: make this dependent on current scrolling window, ONLY draw nodes there (redraw happens anyway!)
        //print nodes of the tree
        for (int i = 0; i < view.nodeLevels.size(); i++)
        {
            List<Integer> nodes = view.nodeLevels.get(i);
            for (int j = 0; j < nodes.size(); j++)
            {
                printBoxAroundNodeTag(cnv, nodes.get(j));               
                printNodeTag(cnv, nodes.get(j));
            }
        }
    }
    
    public void printBoxAroundNodeTag(Graphics2D canvas, int nodeID)
    {
        canvas.setFont(view.getNodeFont(nodeID));
        FontMetrics fm = canvas.getFontMetrics();
        int width = fm.stringWidth(view.getModel().getNodeCaption(nodeID));
        int x = view.getNodeX(nodeID) - width / 2;
        int y = view.getNodeY(nodeID) - view.getNodeHeight(nodeID) + 2;
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
    
    public void printNodeTag(Graphics2D canvas, int nodeID)
    {
        canvas.setFont(view.getNodeFont(nodeID));
        FontMetrics fm = canvas.getFontMetrics();
        String tag = view.getModel().getNodeCaption(nodeID);
        int width = fm.stringWidth(tag);
        canvas.setColor(Color.BLACK);
        //print tag name of node
        int x = view.getNodeX(nodeID) - width / 2;
        int y = view.getNodeY(nodeID);  
        canvas.drawString(tag + " " + nodeID, x, y);
        canvas.setStroke(new BasicStroke(1));
        canvas.setFont(new Font(canvas.getFont().getFontName(),Font.PLAIN, view.getZoomLevel()));
    }
    
    public void printDAGEdges(Graphics canvas)
    {
        // create lines and their tags
        canvas.setColor(Color.BLACK);
        for (int i = 0; i < view.nodeLevels.size(); i++)
        {
            List<Integer> nodes = view.nodeLevels.get(i);
            for (int nodeID : nodes)
            {       
                List<Integer> incomingEdges = view.getModel().getIncomingEdges(nodeID);
                if (incomingEdges.size() > 0)
                {
                    for (int j = 0; j < incomingEdges.size(); j++)
                    {
                        int ancestor = view.getModel().getStartNode(incomingEdges.get(j));
                        if (!view.displaysNode(ancestor))
                        {
                            incomingEdges.addAll(view.getModel().getIncomingEdges(ancestor));
                        }
                        else
                        {
                            int x1 = view.getNodeX(ancestor);
                            int y1 = view.getNodeY(ancestor);
                            int x2 = view.getNodeX(nodeID);
                            int y2 = view.getNodeY(nodeID);
                            canvas.drawLine(x1, y1, x2, y2);
                                    
                            //TODO: add this soon
                            //printEdgeTag(canvas,nodes.get(j),edgyLines);
                            //printEdgeArrow(canvas, nodes.get(j));  
                        }
                    }
                }
            }
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
    
    public void scrollToNode(int nodeID)
    {
        if (nodeID == -1) return;
        Container parent = this.getParent().getParent();
        if (parent instanceof JScrollPane)
        {
            Integer x = view.getNodeX(nodeID);
            Integer y = view.getNodeY(nodeID);
            if (x == null || y == null)
            {
                System.err.println("Could not find scroll coordinates for node " + nodeID);
            }
            else
            {
                SwingUtilities.scrollToCenter((JScrollPane) parent, view.getNodeX(nodeID), view.getNodeY(nodeID));
            }
        }
    }
}
