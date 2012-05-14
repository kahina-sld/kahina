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

import javax.swing.JComponent;

import org.kahina.core.KahinaInstance;
import org.kahina.core.gui.KahinaProgressBar;
import org.kahina.core.task.KahinaTask;
import org.kahina.core.task.KahinaTaskManager;
import org.kahina.core.visual.KahinaViewPanel;

public class KahinaGraphViewPanel extends KahinaViewPanel<KahinaGraphView>
{
    private static final long serialVersionUID = -3000401362714094415L;
    
    protected BufferedImage image;
    
    KahinaGraphViewTaskManager taskManager;
    
    public KahinaGraphViewPanel(KahinaInstance<?, ?, ?> kahina)
    {       
        view = new KahinaGraphView(kahina, new GridLayouter());
        image = new BufferedImage(5, 5, BufferedImage.TYPE_4BYTE_ABGR);
        generateMouseListener();
        taskManager = new KahinaGraphViewTaskManager(this);
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
        if (!view.isVisible()) return;
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
        //repaint();
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
       taskManager.addTask(new PrintGraphVerticesTask(cnv, progressBar, taskManager));
    }
    
    public void printGraphEdges(Graphics canvas)
    {
       taskManager.addTask(new PrintGraphEdgesTask(canvas, progressBar, taskManager));
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
    
    private class PrintGraphEdgesTask extends KahinaTask
    {
        Graphics canvas;
        
        public PrintGraphEdgesTask(Graphics canvas, KahinaProgressBar progressBar, KahinaTaskManager manager)
        {
            super(progressBar,manager);
            this.canvas = canvas;
        }

        @Override
        public void run()
        {
            int currentVertex = 0;
            int edges = 0;
            double numVertices = view.getModel().getVertices().size();
            boolean directed = (view.getConfig().getEdgeInterpretation() == KahinaGraphViewOptions.EDGE_INTERPRETATION_DIRECTED);
            setProgressAndStatus(0, "Drawing graph edges: " + currentVertex + "/" + (int) numVertices + " vertices complete, " + edges + " edges drawn");
            long startTime = System.currentTimeMillis();
            // create lines and their tags
            canvas.setColor(Color.BLACK);
            Set<Integer> processedVertices = new HashSet<Integer>();
            for (int vertex1 : view.getModel().getVertices())
            {
                if (isCanceled()) break;
                int x1 = view.getVertexX(vertex1);
                int y1 = view.getVertexY(vertex1);
                for (int vertex2 : view.getModel().getNeighbors(vertex1))
                {
                    if (vertex2 > vertex1 || directed)
                    {
                        if (view.isEdgeVisible(vertex1,vertex2))
                        {
                            int x2 = view.getVertexX(vertex2);
                            int y2 = view.getVertexY(vertex2);
                            String edgeLabel = view.getModel().getEdgeLabel(vertex1, vertex2);
                            if (edgeLabel.length() > 0)
                            {
                                printEdgeLabel(canvas, new Point((x2+x1)/2,(y2+y1)/2),edgeLabel);
                            }
                            canvas.setColor(view.getEdgeColor(vertex1,vertex2));
                            switch (view.getConfig().getEdgeShapePolicy())
                            {
                                case KahinaGraphViewOptions.EDGE_SHAPE_DIRECT:
                                {
                                    canvas.drawLine(x1, y1, x2, y2);
                                    break;
                                }
                                case KahinaGraphViewOptions.EDGE_SHAPE_RECTANGULAR:
                                {
                                    canvas.drawLine(x1, y1, x2, y1);
                                    canvas.drawLine(x2, y1, x2, y2);
                                    break;
                                }
                                case KahinaGraphViewOptions.EDGE_SHAPE_ARC:
                                {
                                    int halfXDist = (x2 - x1)/2;
                                    int halfYDist = (y2 - y1)/2;
                                    int midPointX = x1 + halfXDist + halfYDist;
                                    int midPointY = y1 + halfYDist - halfXDist;
                                    double radius = Math.sqrt(Math.pow(x1 - midPointX,2) + Math.pow(y1 - midPointY,2));
                                    int startAngle = (int) determineAngleInDegrees(midPointX, midPointY, x1, y1);
                                    int endAngle = (int) determineAngleInDegrees(midPointX, midPointY, x2, y2);
                                    int angleLength = endAngle - startAngle;
                                    if (angleLength < 0)
                                    {
                                        if (angleLength > -180)
                                        {
                                            startAngle = endAngle;
                                            angleLength = -angleLength;
                                        }
                                        else
                                        {
                                            angleLength += 360;
                                        }
                                    }
                                    else if (angleLength > 180)
                                    {
                                        startAngle = endAngle;
                                        angleLength = 360 - angleLength;
                                    }
                                    canvas.drawArc(midPointX - (int) radius, midPointY - (int) radius, (int) radius * 2, (int) radius * 2, startAngle, angleLength);
                                    break;
                                }
                            }
                            edges++;
                            //TODO: add this later (= treatment of directed edges)
                            //printEdgeArrow(canvas, vertex1, vertex2); 
                        }
                    }
                }
                //TODO: make treatment of the vertex agenda dependent on a flag indicating whether the graph is directed
                processedVertices.add(vertex1);
                currentVertex++;
                setProgressAndStatus(currentVertex / numVertices, "Drawing graph edges: " + currentVertex + 
                                                                  "/" + (int) numVertices + " vertices complete, " + edges + " edges drawn");
            }
            setProgressAndStatus(1, "Drawing graph edges: " + (int) numVertices + 
                    "/" + (int) numVertices + " vertices complete, " + edges + " edges drawn");
            System.err.println("  " + edges + " edges in " + (System.currentTimeMillis() - startTime) + " ms.");      
            setFinished();
        }
        
    }
    
    private class PrintGraphVerticesTask extends KahinaTask
    {
        Graphics2D canvas;
        
        public PrintGraphVerticesTask(Graphics2D canvas, KahinaProgressBar progressBar, KahinaTaskManager manager)
        {
            super(progressBar,manager);
            this.canvas = canvas;
        }

        @Override
        public void run()
        {
            int currentVertex = 0;
            int edges = 0;
            double numVertices = view.getModel().getVertices().size();
            setProgressAndStatus(0, "Drawing graph vertices: " + currentVertex + "/" + numVertices + " vertices complete.");
            long startTime = System.currentTimeMillis();
            // create lines and their tags
            canvas.setColor(Color.BLACK);
            Set<Integer> processedVertices = new HashSet<Integer>();
            for (int vertex : view.getModel().getVertices())
            {
                if (isCanceled()) break;
                printGraphVertex(canvas,vertex);
                currentVertex++;
                setProgressAndStatus(currentVertex / numVertices, "Drawing graph vertices: " + currentVertex + 
                                                                  "/" + (int) numVertices + " vertices complete.");
            }
            setProgressAndStatus(1, "Drawing graph vertices: " + (int) numVertices + 
                    "/" + (int) numVertices + " vertices complete.");
            System.err.println("  " + currentVertex + " vertices in " + (System.currentTimeMillis() - startTime) + " ms.");      
            setFinished();
        }
        
    }
    
    private double determineAngleInDegrees(double midpointX, double midpointY, double pointX, double pointY)
    {
        //System.err.print("angleInDegrees[(" + midpointX + "," + midpointY + ") -> (" + pointX + "," + pointY + ")] = ");
        if (midpointX == pointX)
        {
            if (midpointY  < pointY)
            {
                return 270;
            }
            else if (midpointY > pointY)
            {
                return 90;
            }
            //is actually undefined, but 0 is a safe default value for our purposes
            return 0;
        }
        else if (midpointX < pointX)
        {
            if (midpointY >= pointY)
            {
                //System.err.println(180 * Math.atan((midpointY - pointY) / (pointX - midpointX)) / Math.PI);
                return 180 * Math.atan((midpointY - pointY) / (pointX - midpointX)) / Math.PI;
            }
            else
            {
                //System.err.println(360 - 180 * Math.atan((midpointY - pointY) / (pointX - midpointX)) / Math.PI);
                return 360 + 180 * Math.atan((midpointY - pointY) / (pointX - midpointX)) / Math.PI;
            }
        }
        else
        {
            //System.err.println(180 + 180 * Math.atan((midpointY - pointY) / (pointX - midpointX)) / Math.PI);
            return 180 + 180 * Math.atan((midpointY - pointY) / (pointX - midpointX)) / Math.PI;
        }
    }
}
