package org.kahina.core.visual.chart;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.tree.KahinaTreeViewOptions;

public class KahinaRecursiveChartViewPanel extends KahinaChartViewPanel
{
    KahinaRecursiveChartView view;
    
    public KahinaRecursiveChartViewPanel(KahinaInstance<?, ?, ?, ?> kahina)
    {
        super(kahina);
        view = null;
    }
    
    public void setView(KahinaRecursiveChartView view)
    {
        this.view = view;
        super.setView(view);
    }

    @Override
    public void updateDisplay()
    {
        if (view == null) return;
        image = new BufferedImage(view.getDisplayWidth() + 8, view.getDisplayHeight() + 5 + 2 * view.config.getZoomLevel(), BufferedImage.TYPE_4BYTE_ABGR);
        Graphics canvas = image.getGraphics();
        Graphics2D cnv = (Graphics2D) canvas;
        
        if (view.config.getAntialiasingPolicy() == KahinaTreeViewOptions.ANTIALIASING)
        {
            cnv.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        }
        
        //determine font size
        int fontSize = view.config.getZoomLevel();
        Font font = new Font("Arial", Font.PLAIN, fontSize);
        canvas.setFont(font);
        int maxX = view.getDisplayWidth() + 4;
        int maxY = view.getDisplayHeight();
        clearCanvas(canvas);
        
        //cosmetic improvement of frame
        cnv.setColor(Color.BLACK);
        cnv.setStroke(new BasicStroke(2));
        cnv.drawLine(5, 5, 5, maxY + 6);
        cnv.drawLine(maxX + 1, 5, maxX + 1, maxY + 6);
        cnv.drawLine(5, 5, maxX, 5);
        cnv.drawLine(5, maxY + 6, maxX, maxY + 6);
        
        //draw the recursively nested edges from large to small
        for (int rootEdge : view.getModel().getDependencyRoots())
        {
            recursivelyDrawEdge(rootEdge, 0, 0, cnv);        
        }
        
        //draw segment captions
        cnv.setStroke(new BasicStroke(1));
        cnv.setFont(new Font(Font.MONOSPACED,Font.PLAIN, view.config.getZoomLevel()));
        cnv.setColor(Color.BLACK);
        
        for (Integer i : view.getModel().getSegmentsWithCaption())
        {
            if (view.getSegmentWidth(i) > 0)
            {
                cnv.drawString(i + " " + view.getSegmentCaption(i), view.getSegmentOffset(i) + 5, maxY + view.config.getZoomLevel() + 6);
            }
        }  
        
        revalidate();  
    }
    
    public void recursivelyDrawEdge(int parentEdge, int relX, int relY, Graphics2D cnv)
    {
        System.err.println("recursivelyDrawEdge(" + parentEdge + "," + relX + "," + relY + ")");
        drawEdge(relX, relY, parentEdge, cnv);
        for (int daughterEdge : view.getModel().getDaughterEdgesForEdge(parentEdge))
        {
            int daughterRelX = relX +  view.getEdgeX(parentEdge); 
            int daughterRelY = relY +  view.getEdgeY(parentEdge); 
            recursivelyDrawEdge(daughterEdge, daughterRelX, daughterRelY, cnv);
        }
    }
}
