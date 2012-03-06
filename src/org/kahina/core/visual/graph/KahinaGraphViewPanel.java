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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.KahinaViewPanel;

public class KahinaGraphViewPanel extends KahinaViewPanel<KahinaGraphView>
{
    private static final long serialVersionUID = -3000401362714094415L;
    
    BufferedImage image;
    
    public KahinaGraphViewPanel(KahinaController control)
    {       
        view = new KahinaGraphView(control, new GridLayouter());
        image = new BufferedImage(5, 5, BufferedImage.TYPE_4BYTE_ABGR);
        generateMouseListener();
    }
    
    protected void generateMouseListener()
    {
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
        long startTime = System.currentTimeMillis();
        List<Integer> redrawAgenda = view.getRedrawAgenda();
        System.err.println("Updating graph display of size " + view.getDisplayWidth() + " * " + view.getDisplayHeight());
        if (!redrawAgenda.isEmpty())
        {
            if (redrawAgenda.get(0) == -1)
            {
                BufferedImage newImage = new BufferedImage(view.getDisplayWidth() + 1, view.getDisplayHeight() + 1, BufferedImage.TYPE_4BYTE_ABGR);
                Graphics cnv = newImage.getGraphics();
                Graphics2D canvas = (Graphics2D) cnv;
                if (view.getConfig().getAntialiasingPolicy() == KahinaGraphViewOptions.ANTIALIASING)
                {
                    canvas.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                }
                //determine font size
                int fontSize = view.getConfig().getNodeSize();
                Font font = new Font("Arial", Font.PLAIN, fontSize);
                canvas.setFont(font);
        
                clearCanvas(canvas);
                
                canvas.setStroke(new BasicStroke(1));
                canvas.setColor(Color.BLACK);
                if (view.getConfig().getDrawingOrderPolicy() == KahinaGraphViewOptions.VERTICES_ABOVE_EDGES)
                {
                    printGraphEdges(canvas);
                    printGraphVertices(canvas);
                }
                else
                {
                    printGraphVertices(canvas);
                    printGraphEdges(canvas);
                }
                
                image = newImage;
                redrawAgenda.clear();
            }
            else
            {
                Graphics2D canvas = (Graphics2D) image.getGraphics();
                //set default rendering options
                if (view.getConfig().getAntialiasingPolicy() == KahinaGraphViewOptions.ANTIALIASING)
                {
                    canvas.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                }
                int fontSize = view.getConfig().getNodeSize();
                Font font = new Font("Arial", Font.PLAIN, fontSize);
                canvas.setFont(font);
                canvas.setStroke(new BasicStroke(1));
                canvas.setColor(Color.BLACK);
                for (int vertex : redrawAgenda)
                {
                    if (view.getConfig().getDrawingOrderPolicy() == KahinaGraphViewOptions.VERTICES_ABOVE_EDGES)
                    {
                        printEdgesForVertex(canvas, vertex);
                        printGraphVertex(canvas, vertex);
                    }
                    else
                    {
                        printGraphVertex(canvas, vertex);
                        printEdgesForVertex(canvas, vertex);
                    }
                }
                redrawAgenda.clear();
            }
        }
        repaint();
        System.err.println("  Finished updating! Total time spent: " + (System.currentTimeMillis() - startTime) + " ms.");
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
        long startTime = System.currentTimeMillis();
        int vertices = 0;
        for (int vertex : view.getModel().getVertices())
        {
           vertices++;
           printGraphVertex(cnv,vertex);
        }
        System.err.println("  " + vertices + " vertices in " + (System.currentTimeMillis() - startTime) + " ms.");
    }
    
    public void printGraphVertex(Graphics2D cnv, int vertex)
    {
        if (view.isVertexVisible(vertex))
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
        int size = view.getConfig().getNodeSize();
        int x = view.getVertexX(vertex) - size / 2;
        int y = view.getVertexY(vertex) - size / 2;
        Color color = view.getVertexColor(vertex);
        if (color != null)
        { 

            canvas.setColor(color);
            canvas.fillOval(x, y, size, size);
            if (view.getVertexBorderColor(vertex) != null)
            {
                canvas.setColor(view.getVertexBorderColor(vertex));
                canvas.drawOval(x - 1, y - 1, size + 2, size + 2);
            }
        }
    }
    
    public void printGraphEdges(Graphics canvas)
    {
        long startTime = System.currentTimeMillis();
        // create lines and their tags
        canvas.setColor(Color.BLACK);
        int edges = 0;
        Set<Integer> processedVertices = new HashSet<Integer>();
        for (int vertex1 : view.getModel().getVertices())
        {
            if (view.isVertexVisible(vertex1))
            {
                int x1 = view.getVertexX(vertex1);
                int y1 = view.getVertexY(vertex1);
                //TODO: treat undirected edges more efficiently (they are currently drawn twice!)
                for (int vertex2 : view.getModel().getNeighbors(vertex1))
                {
                    if (!processedVertices.contains(vertex2))
                    {
                        if (view.isVertexVisible(vertex2))
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
                            edges++;
                            //TODO: add this later (= treatment of directed edges)
                            //printEdgeArrow(canvas, vertex1, vertex2); 
                        }
                        else
                        {
                            processedVertices.add(vertex2);
                        }
                    }
                }
            }
            //TODO: make treatment of the vertex agenda dependent on a flag indicating whether the graph is directed
            processedVertices.add(vertex1);
        }
        System.err.println("  " + edges + " edges in " + (System.currentTimeMillis() - startTime) + " ms.");
    }
    
    public void printEdgesForVertex(Graphics canvas, int vertex1)
    {
        if (view.isVertexVisible(vertex1))
        {
            int x1 = view.getVertexX(vertex1);
            int y1 = view.getVertexY(vertex1);
            for (int vertex2 : view.getModel().getNeighbors(vertex1))
            {
                if (view.isVertexVisible(vertex2))
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
                    //TODO: add this later (= treatment of directed edges)
                    //printEdgeArrow(canvas, vertex1, vertex2); 
                }
            }
        }
    }
    
    public void printEdgeLabel(Graphics canvas, Point center, String label)
    {
        int edgeLabels = 0;
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
            edgeLabels++;
        }
    }
}
