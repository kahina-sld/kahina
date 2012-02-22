package org.kahina.core.visual.graph;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;

import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.core.visual.tree.KahinaTreeViewOptions;

public class KahinaGraphViewPanel extends KahinaViewPanel<KahinaGraphView>
{
    private static final long serialVersionUID = -3000401362714094415L;
    
    BufferedImage image;
    
    public KahinaGraphViewPanel(KahinaController control)
    {       
        view = new KahinaGraphView(control, new GridLayouter());
        image = new BufferedImage(5, 5, BufferedImage.TYPE_4BYTE_ABGR);
        this.addMouseListener(new KahinaGraphViewListener(this));
    }
    
    @Override
    public void paintComponent(Graphics cnv)
    {
        try
        {
            Thread.sleep(10);
            super.paintComponent(cnv);
            if (image == null) 
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
        System.err.println("Update graph display of size " + view.getDisplayWidth() + " * " + view.getDisplayHeight());
        BufferedImage newImage = new BufferedImage(view.getDisplayWidth() + 1, view.getDisplayHeight() + 1, BufferedImage.TYPE_4BYTE_ABGR);
        Graphics cnv = newImage.getGraphics();
        Graphics2D canvas = (Graphics2D) cnv;
        if (view.getConfig().getAntialiasingPolicy() == KahinaGraphViewOptions.ANTIALIASING)
        {
            canvas.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        }
        //determine font size
        int fontSize = view.getConfig().getZoomLevel();
        Font font = new Font("Arial", Font.PLAIN, fontSize);
        canvas.setFont(font);

        clearCanvas(canvas);
        
        canvas.setStroke(new BasicStroke(1));
        canvas.setColor(Color.BLACK);
        printGraphEdges(canvas);
        printGraphVertices(canvas);
        
        image = newImage;
        repaint();
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
        //hack to account for small graph
        canvas.fillRect(0, 0, 2000, 2000);
        canvas.fillRect(0, 0, this.getSize().width, this.getSize().height);
    }  
    
    public void printGraphVertices(Graphics2D cnv)
    {
        //TODO: make this dependent on current scrolling window, ONLY draw nodes there (redraw happens anyway!)
        //print nodes of the graph
        //TODO: implement the concept of a drawing order in order to control which parts are printed in front
        for (int vertex : view.getModel().getVertices())
        {
            if (view.getConfig().getVertexShapePolicy() == KahinaGraphViewOptions.BOX_VERTICES)
            {
                printBoxAroundVertexLabel(cnv, vertex);    
                printVertexLabel(cnv, vertex);
            }
            else if (view.getConfig().getVertexShapePolicy() == KahinaGraphViewOptions.OVAL_VERTICES)
            {
                printOvalAroundVertexLabel(cnv, vertex);
                printVertexLabel(cnv, vertex);
            }
            else if (view.getConfig().getVertexShapePolicy() == KahinaGraphViewOptions.POINT_VERTICES)
            {
                printVertexPoint(cnv, vertex);
            }
        }
    }
    
    public void printBoxAroundVertexLabel(Graphics2D canvas, int vertex)
    {
        canvas.setFont(view.getVertexFont(vertex));
        FontMetrics fm = canvas.getFontMetrics();
        int width = fm.stringWidth(view.getModel().getVertexLabel(vertex));
        int x = view.getVertexX(vertex) - width / 2;
        int y = view.getVertexY(vertex) - view.getVertexHeight(vertex) + 2;
        Color color = view.getVertexColor(vertex);
        if (color != null)
        { 
            canvas.setColor(color);
            canvas.fillRect(x - 2, y, width + 4, view.getVertexHeight(vertex) + 2);
            canvas.setColor(Color.BLACK);
            canvas.drawRect(x - 2, y, width + 4, view.getVertexHeight(vertex) + 2);
            if (view.getVertexBorderColor(vertex) != null)
            {
                canvas.setColor(view.getVertexBorderColor(vertex));
                canvas.setStroke(new BasicStroke(2));
                canvas.drawRect(x - 3, y - 1, width + 6, view.getVertexHeight(vertex) + 4);
            }
        }
    }
    
    public void printOvalAroundVertexLabel(Graphics2D canvas, int vertex)
    {
        canvas.setFont(view.getVertexFont(vertex));
        FontMetrics fm = canvas.getFontMetrics();
        int width = fm.stringWidth(view.getModel().getVertexLabel(vertex));
        int x = view.getVertexX(vertex) - width / 2;
        int y = view.getVertexY(vertex) - view.getVertexHeight(vertex) + 2;
        Color color = view.getVertexColor(vertex);
        if (color != null)
        { 
            canvas.setColor(color);
            canvas.fillOval(x - 2, y, width + 4, view.getVertexHeight(vertex) + 4);
            canvas.setColor(Color.BLACK);
            canvas.drawOval(x - 2, y, width + 4, view.getVertexHeight(vertex) + 4);
            if (view.getVertexBorderColor(vertex) != null)
            {
                canvas.setColor(view.getVertexBorderColor(vertex));
                canvas.setStroke(new BasicStroke(2));
                canvas.drawOval(x - 3, y - 1, width + 6, view.getVertexHeight(vertex) + 6);
            }
        }
    }
    
    public void printVertexLabel(Graphics2D canvas, int vertex)
    {
        canvas.setFont(view.getVertexFont(vertex));
        FontMetrics fm = canvas.getFontMetrics();
        String tag = view.getModel().getVertexLabel(vertex);
        int width = fm.stringWidth(tag);
        canvas.setColor(Color.BLACK);
        //print tag name of node
        int x = view.getVertexX(vertex) - width / 2;
        int y = view.getVertexY(vertex);
        canvas.drawString(tag, x, y);
        canvas.setStroke(new BasicStroke(1));
        canvas.setFont(new Font(canvas.getFont().getFontName(),Font.PLAIN, view.getConfig().getZoomLevel()));
    }
    
    public void printVertexPoint(Graphics2D canvas, int vertex)
    {
        int x = view.getVertexX(vertex);
        int y = view.getVertexY(vertex);
        Color color = view.getVertexColor(vertex);
        if (color != null)
        { 
            canvas.setColor(color);
            canvas.fillOval(x, y, 2, 2);
            if (view.getVertexBorderColor(vertex) != null)
            {
                canvas.setColor(view.getVertexBorderColor(vertex));
                canvas.drawOval(x - 1, y - 1, 4, 4);
            }
        }
    }
    
    public void printGraphEdges(Graphics canvas)
    {
        // create lines and their tags
        canvas.setColor(Color.BLACK);
        for (int vertex1 : view.getModel().getVertices())
        {

            int x1 = view.getVertexX(vertex1);
            int y1 = view.getVertexY(vertex1);
            //TODO: treat undirected edges more efficiently (they are currently drawn twice!)
            for (int vertex2 : view.getModel().getNeighbors(vertex1))
            {
                int x2 = view.getVertexX(vertex2);
                int y2 = view.getVertexY(vertex2);
                String edgeLabel = view.getModel().getEdgeLabel(vertex1, vertex2);
                if (edgeLabel.length() > 0)
                {
                    printEdgeLabel(canvas, new Point((x2+x1)/2,(y2+y1)/2),edgeLabel);
                }
                canvas.setColor(view.getEdgeColor(vertex1,vertex2));
                canvas.drawLine(x1, y1, x2, y2);
                //TODO: add this later
                //printEdgeArrow(canvas, vertex1, vertex2);   
            }
            System.err.println("Finished drawing " + view.getModel().getNeighbors(vertex1).size() + " lines starting at vertex " + vertex1);
        }
    }
    
    public void printEdgeLabel(Graphics canvas, Point center, String label)
    {
        if (label != null && view.getConfig().getEdgeLabelPolicy() != KahinaGraphViewOptions.NO_EDGE_LABELS)
        {
            FontMetrics fm = canvas.getFontMetrics();
            int width = fm.stringWidth(label);
            int height = fm.getHeight();
            canvas.setColor(Color.BLACK);
            //print edge tag
            int x = center.x - width / 2;
            int y = center.y;
            if (view.getConfig().getEdgeLabelPolicy() == KahinaGraphViewOptions.OVAL_EDGE_LABELS)
            {
                canvas.setColor(Color.WHITE);
                canvas.fillOval(x - 2, y - height + 2, width + 4, height + 2);
                canvas.setColor(Color.BLACK);
                canvas.drawOval(x - 2, y - height + 2, width + 4, height + 2);
            }
            else if (view.getConfig().getEdgeLabelPolicy() == KahinaGraphViewOptions.BOXED_EDGE_LABELS)
            {
                canvas.setColor(Color.WHITE);
                canvas.fillRect(x - 2, y - height + 2, width + 4, height + 2);
                canvas.setColor(Color.BLACK);
                canvas.drawRect(x - 2, y - height + 2, width + 4, height + 2);
            }
            canvas.drawString(label, x, y);
        }
    }
}
