package org.kahina.core.visual.chart;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;

import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.core.visual.tree.KahinaTreeView;

public class KahinaChartViewPanel extends KahinaViewPanel<KahinaChartView>
{
	private static final long serialVersionUID = -1083038134731774917L;
	
	BufferedImage image;
    
    public KahinaChartViewPanel()
    {
        view = null;
        image = new BufferedImage(5, 5, BufferedImage.TYPE_4BYTE_ABGR);
        this.addMouseListener(new KahinaChartViewListener(this));
    }
    
    public void paintComponent(Graphics canvas)
    {
        try
        {
            Thread.sleep(10);
            if ( image == null ) 
            {
                return;
            }
            canvas.drawImage(image, 0, 0, this);
        }
        catch (InterruptedException e)
        {
            System.err.println("Sleep interrupted!");
        }    
    }
    
    public void updateDisplay()
    {
        if (view == null) return;
        image = new BufferedImage(view.getDisplayWidth() + 8, view.getDisplayHeight() + 5 + 2 * view.getZoomLevel(), BufferedImage.TYPE_4BYTE_ABGR);
        Graphics canvas = image.getGraphics();
        Graphics2D cnv = (Graphics2D) canvas;
        
        if (view.getAntialiasingPolicy() == KahinaTreeView.ANTIALIASING)
        {
            cnv.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        }
        
        //determine font size
        int fontSize = view.getZoomLevel();
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
        
        //draw all the edges
        for (Integer id : view.getEdgeIDs())
        {
            //store edge information that is used multiple times
            int x = view.getEdgeX(id);
            int y = view.getEdgeY(id);
            int width = view.getEdgeWidth(id);
            int height = view.getEdgeHeight(id);    
            
            //System.err.println("Edge " + id + ": x=" + x + " y=" + y + " width=" + width + " height=" + height);

            //paint edge background in appropriate colour
            cnv.setColor(view.getEdgeColor(id));
            cnv.fillRect(x + 5, y + 5, width, height);
            
            //paint edge rim and caption as desired
            if (view.getMarkedEdge() == id)
            {
                cnv.setColor(Color.YELLOW);
                cnv.setStroke(new BasicStroke(2));
            }
            else
            {
                cnv.setColor(Color.BLACK);
                cnv.setStroke(view.getEdgeStroke(id));
            }
            cnv.setFont(view.getEdgeFont(id)); 
            cnv.drawRect(x + 5, y + 5, width, height);
            cnv.setColor(Color.BLACK);
            //cnv.drawString(view.getEdgeCaption(id), x + 7, y + view.fontSize + 6);
            //debugging version
            cnv.drawString(view.getEdgeCaption(id) + "(" + view.getModel().getLeftBoundForEdge(id) + "," + view.getModel().getRightBoundForEdge(id) + ")", x + 7, y + view.fontSize + 6);
        }
        
        //draw segment captions
        cnv.setStroke(new BasicStroke(1));
        cnv.setFont(new Font(Font.MONOSPACED,Font.PLAIN, view.fontSize));
        cnv.setColor(Color.BLACK);
        
        for (Integer i : view.getModel().getSegmentsWithCaption())
        {
            if (view.getSegmentWidth(i) > 0)
            {
                cnv.drawString(i + " " + view.getSegmentCaption(i), view.getSegmentOffset(i) + 5, maxY + view.fontSize + 6);
            }
        }    
        
        //TODO: introduce advanced display options:
        // * only display edges with certain status values
        // * allow adjustment of selective status display via menus
        // * should allow names for status (visibility menu then generated from it)
        // * define status changes as reaction to clicks (and allow more hooks for interaction)
    }
    
    public void clearCanvas(Graphics canvas)
    {
        // clear canvas
        Dimension newD = new Dimension(view.getDisplayWidth() + 8, view.getDisplayHeight() + 5 + 2 * view.getZoomLevel());
        this.setSize(newD);
        this.setMinimumSize(newD);
        this.setMaximumSize(newD);
        this.setPreferredSize(newD);
        this.setBackground(view.bgColor);
        canvas.setColor(view.bgColor);
        //little hack to account for small charts
        canvas.fillRect(0, 0, 2000, 2000);
        canvas.fillRect(0, 0, this.getSize().width, this.getSize().height);
    }
}
