package org.kahina.core.visual.chart;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;

import org.kahina.core.data.chart.KahinaChart;
import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.core.visual.tree.KahinaTreeView;

public class KahinaChartViewPanel extends KahinaViewPanel<KahinaChart>
{
    BufferedImage image;
    KahinaChartView v;
    
    public KahinaChartViewPanel()
    {
        v = new KahinaChartView();
        image = new BufferedImage(5, 5, BufferedImage.TYPE_4BYTE_ABGR);
        this.addMouseListener(new KahinaChartViewListener(this));
    }
    
    public KahinaChartViewPanel(KahinaChartViewMarker marker)
    {
        v = new KahinaChartView();
        image = new BufferedImage(5, 5, BufferedImage.TYPE_4BYTE_ABGR);
        this.addMouseListener(new KahinaChartViewListener(this, marker));
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
        if (v == null) return;
        image = new BufferedImage(v.getDisplayWidth() + 8, v.getDisplayHeight() + 5 + 2 * v.getZoomLevel(), BufferedImage.TYPE_4BYTE_ABGR);
        Graphics canvas = image.getGraphics();
        Graphics2D cnv = (Graphics2D) canvas;
        
        if (v.getAntialiasingPolicy() == KahinaTreeView.ANTIALIASING)
        {
            cnv.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        }
        
        //determine font size
        int fontSize = v.getZoomLevel();
        Font font = new Font("Arial", Font.PLAIN, fontSize);
        canvas.setFont(font);
        int maxX = v.getDisplayWidth() + 4;
        int maxY = v.getDisplayHeight();
        clearCanvas(canvas);
        
        //cosmetic improvement of frame
        cnv.setColor(Color.BLACK);
        cnv.setStroke(new BasicStroke(2));
        cnv.drawLine(5, 5, 5, maxY + 6);
        cnv.drawLine(maxX + 1, 5, maxX + 1, maxY + 6);
        cnv.drawLine(5, 5, maxX, 5);
        cnv.drawLine(5, maxY + 6, maxX, maxY + 6);
        
        //draw all the edges
        for (Integer id : v.getEdgeIDs())
        {
            //store edge information that is used multiple times
            int x = v.getEdgeX(id);
            int y = v.getEdgeY(id);
            int width = v.getEdgeWidth(id);
            int height = v.getEdgeHeight(id);    
            
            //System.err.println("Edge " + id + ": x=" + x + " y=" + y + " width=" + width + " height=" + height);

            //paint edge background in appropriate colour
            cnv.setColor(v.getEdgeColor(id));
            cnv.fillRect(x + 5, y + 5, width, height);
            
            //paint edge rim and caption as desired
            cnv.setColor(Color.BLACK);
            cnv.setStroke(v.getEdgeStroke(id));
            cnv.setFont(v.getEdgeFont(id)); 
            cnv.drawRect(x + 5, y + 5, width, height);
            cnv.drawString(v.getEdgeCaption(id), x + 7, y + v.fontSize + 6);
        }
        
        //draw segment captions
        cnv.setStroke(new BasicStroke(1));
        cnv.setFont(new Font(Font.MONOSPACED,Font.PLAIN, v.fontSize));
        cnv.setColor(Color.BLACK);
        
        for (Integer i : v.getModel().getSegmentsWithCaption())
        {
            if (v.getSegmentWidth(i) > 0)
            {
                cnv.drawString(i + " " + v.getSegmentCaption(i), v.getSegmentOffset(i) + 5, maxY + v.fontSize + 6);
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
        Dimension newD = new Dimension(v.getDisplayWidth() + 8, v.getDisplayHeight() + 5 + 2 * v.getZoomLevel());
        this.setSize(newD);
        this.setMinimumSize(newD);
        this.setMaximumSize(newD);
        this.setPreferredSize(newD);
        this.setBackground(v.bgColor);
        canvas.setColor(v.bgColor);
        //little hack to account for small charts
        canvas.fillRect(0, 0, 2000, 2000);
        canvas.fillRect(0, 0, this.getSize().width, this.getSize().height);
    }
}
