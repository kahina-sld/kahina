package org.kahina.core.visual.chart;

import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

public class KahinaChartViewListener extends MouseAdapter implements ActionListener
{
    KahinaChartViewPanel view;
    
    KahinaChartViewMarker marker;
    
    public KahinaChartViewListener(KahinaChartViewPanel view)
    {
        this.view = view;
        //separate empty model: marking clicks will not have any effect
        this.marker = new KahinaChartViewMarker(view.v.getModel());
    }
    
    public KahinaChartViewListener(KahinaChartViewPanel view, KahinaChartViewMarker m)
    {
        this.view = view;
        this.marker = m;
    }
    
    public void mouseClicked(MouseEvent e)
    {
        int clickedEdge = view.v.edgeAtCoordinates(e.getX() - 5, e.getY() - 5);
        marker.markEdge(clickedEdge);
        view.updateDisplay();
        view.repaint();
    }
    
    public void mousePressed(MouseEvent e) 
    {
        maybeShowPopup(e);
    }

    public void mouseReleased(MouseEvent e) 
    {
        maybeShowPopup(e);
    }

    private void maybeShowPopup(MouseEvent e) 
    {
        if (e.isPopupTrigger()) 
        {
            KahinaChartViewContextMenu.getMenu(this, view.v).show(e.getComponent(),e.getX(), e.getY());
        }
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String command = e.getActionCommand();
        if (command.equals("Zoom In"))
        {
            view.v.zoomIn();
        }
        else if (command.equals("Zoom Out"))
        {
            view.v.zoomOut();
        }
        else if (command.equals("Minimal necessary width"))
        {
            view.v.setCellWidthPolicy(KahinaChartView.MINIMAL_NECESSARY_WIDTH);
        }
        else if (command.equals("Maximal necessary width"))
        {
            view.v.setCellWidthPolicy(KahinaChartView.MAXIMAL_NECESSARY_WIDTH);
        }
        else if (command.equals("Fixed width"))
        {
            view.v.setCellWidthPolicy(KahinaChartView.FIXED_WIDTH);
        }
        else if (command.equals("Fill space compactly"))
        {
            view.v.setEdgeStackingPolicy(KahinaChartView.STACK_EDGES_FILL_SPACE);
        }
        else if (command.equals("Maintain chronological order"))
        {
            view.v.setEdgeStackingPolicy(KahinaChartView.STACK_EDGES_BY_ID);
        }
        else if (command.equals("Bottom Up"))
        {
            view.v.setDisplayOrientation(KahinaChartView.BOTTOM_UP_DISPLAY);
        }
        else if (command.equals("Top Down"))
        {
            view.v.setDisplayOrientation(KahinaChartView.TOP_DOWN_DISPLAY);
        }
        else if (command.equals("Used range only"))
        {
            view.v.setDisplayRangePolicy(KahinaChartView.RANGE_USED_ONLY);
        }
        else if (command.equals("Defined range"))
        {
            view.v.setDisplayRangePolicy(KahinaChartView.RANGE_USED_OR_CAPTION_DEFINED);
        }
        else if (command.equals("Complete range"))
        {
            view.v.setDisplayRangePolicy(KahinaChartView.RANGE_COMPLETE);
        }
        else if (command.equals("Antialiasing On"))
        {
            view.v.setAntialiasingPolicy(KahinaChartView.ANTIALIASING);
        }
        else if (command.equals("Antialiasing Off"))
        {
            view.v.setAntialiasingPolicy(KahinaChartView.NO_ANTIALIASING);
        }
        else if (command.endsWith("0 %"))
        {
            int zoomLevel = Integer.parseInt(command.substring(0, command.length() - 3));
            view.v.setZoomLevel(zoomLevel);
        }
        else if (command.equals("Save as PNG"))
        {
            JFileChooser chooser = new JFileChooser(new File("."));
            //FileNameExtensionFilter filter = new FileNameExtensionFilter("PNG Images", "png");
            //chooser.setFileFilter(filter);
            chooser.showSaveDialog(view);
            File outputFile = chooser.getSelectedFile();
            
            BufferedImage outputImage = new BufferedImage(view.v.getDisplayWidth() + 8, view.v.getDisplayHeight() + view.v.cellHeight + 8, BufferedImage.TYPE_INT_ARGB);
            Graphics outputCanvas = outputImage.getGraphics();
            view.paint(outputCanvas);
            try
            {
                ImageIO.write(outputImage,"png",outputFile);
            }
            catch (IOException ioe)
            {
                JOptionPane.showMessageDialog(view, ioe.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
            }
        }
        view.updateDisplay();
        view.repaint();
    }

}
