package org.kahina.core.visual.dag;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.util.List;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.tree.KahinaTreeViewOptions;

public class ColoredPathDAGViewPanel extends KahinaDAGViewPanel
{
    ColoredPathDAGView view;
    
    public ColoredPathDAGViewPanel(KahinaInstance<?, ?, ?, ?> kahina)
    {
        super(kahina);
        view = new ColoredPathDAGView(kahina, new LayeredLayouter());
    }
    
    public void setView(ColoredPathDAGView view)
    {
        super.setView(view);
        this.view = view;
        updateDisplayAndRepaintFromEventDispatchThread();
    }
    
    //TODO: refactor to avoid the code duplication (make printDAGEdges abstract?)
    public void updateDisplay()
    {      
        kahina.getLogger().startMeasuring();
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
        printDAGEdges(canvas);
        printDAGNodes(canvas);
        
        image = newImage;
        repaint();
        
        //TODO: improve behavior when View is smaller than viewport (e.g. add dummy material)
        //scrollToNode(view.getMarkedNode());
        revalidate();
        
        /*for (TreeViewExtension ext : viewExtensionsAfterMainRendering)
        {
            ext.paintOnTreePanel(this, canvas);
        }*/
        kahina.getLogger().endMeasuring("for updating " + this);
    }

    public void printDAGEdges(Graphics canvas)
    {
        // create lines and their tags
        //System.err.println("KahinaColoredDAGViewPanel.printDAGEdges");
        for (int nodeID : view.getModel().getNodeIDIterator())
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
                        int y1 = view.getNodeY(ancestor) + view.getConfig().getNodeSize();
                        int x2 = view.getNodeX(nodeID);
                        int y2 = view.getNodeY(nodeID);
                        List<Color> edgeColors = view.getEdgeColors(incomingEdges.get(j));
                        int xOffset = edgeColors.size();
                        if (xOffset == 0)
                        {
                            canvas.setColor(Color.BLACK);
                            canvas.drawLine(x1, y1, x2, y2);
                        }
                        else
                        {
                            for (int i = - xOffset; i < xOffset; i += 2)
                            {
                                canvas.setColor(edgeColors.get((i + xOffset) / 2));
                                canvas.drawLine(x1 + i, y1, x2 + i, y2);
                                canvas.drawLine(x1 + i + 1, y1, x2 + i + 1, y2);
                            } 
                            printEdgeLabel(canvas, new Point((x1 + x2)/2, (y1 + y2)/2), view.getModel().getEdgeLabel(incomingEdges.get(j)));
                            //TODO: add this soon
                            //printEdgeArrow(canvas, nodes.get(j));  
                        }
                    }
                }
            }
        }
        canvas.setColor(Color.BLACK);
    }
}
