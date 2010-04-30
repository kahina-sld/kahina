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

import org.kahina.core.KahinaRunner;
import org.kahina.core.gui.event.KahinaEdgeSelectionEvent;

public class KahinaChartViewListener extends MouseAdapter implements ActionListener
{
    KahinaChartViewPanel view;
    
    public KahinaChartViewListener(KahinaChartViewPanel view)
    {
        this.view = view;
    }
    
    public void mouseClicked(MouseEvent e)
    {
        int clickedEdge = view.view.edgeAtCoordinates(e.getX() - 5, e.getY() - 5);
        //marking and redrawing happens indirectly
        KahinaRunner.processEvent(new KahinaEdgeSelectionEvent(clickedEdge));
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
            KahinaChartViewContextMenu.getMenu(this, view.view).show(e.getComponent(),e.getX(), e.getY());
        }
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String command = e.getActionCommand();
        if (command.equals("Zoom In"))
        {
            view.view.zoomIn();
        }
        else if (command.equals("Zoom Out"))
        {
            view.view.zoomOut();
        }
        else if (command.equals("Minimal necessary width"))
        {
            view.view.setCellWidthPolicy(KahinaChartView.MINIMAL_NECESSARY_WIDTH);
        }
        else if (command.equals("Maximal necessary width"))
        {
            view.view.setCellWidthPolicy(KahinaChartView.MAXIMAL_NECESSARY_WIDTH);
        }
        else if (command.equals("Fixed width"))
        {
            view.view.setCellWidthPolicy(KahinaChartView.FIXED_WIDTH);
        }
        else if (command.equals("Ancestors and Descendants"))
        {
            view.view.setDependencyDisplayPolicy(KahinaChartView.BOTH_ANCESTORS_AND_DESCENDANTS);
        }
        else if (command.equals("Only ancestors (= production)"))
        {
            view.view.setDependencyDisplayPolicy(KahinaChartView.ANCESTORS_ONLY);
        }
        else if (command.equals("Only descendants (= origin)"))
        {
            view.view.setDependencyDisplayPolicy(KahinaChartView.DESCENDANTS_ONLY);
        }
        else if (command.equals("No dependencies"))
        {
            view.view.setDependencyDisplayPolicy(KahinaChartView.NO_DEPENDENCIES);
        }
        else if (command.equals("Fill space compactly"))
        {
            view.view.setEdgeStackingPolicy(KahinaChartView.STACK_EDGES_FILL_SPACE);
        }
        else if (command.equals("Maintain chronological order"))
        {
            view.view.setEdgeStackingPolicy(KahinaChartView.STACK_EDGES_BY_ID);
        }
        else if (command.equals("Bottom Up"))
        {
            view.view.setDisplayOrientation(KahinaChartView.BOTTOM_UP_DISPLAY);
        }
        else if (command.equals("Top Down"))
        {
            view.view.setDisplayOrientation(KahinaChartView.TOP_DOWN_DISPLAY);
        }
        else if (command.equals("Used range only"))
        {
            view.view.setDisplayRangePolicy(KahinaChartView.RANGE_USED_ONLY);
        }
        else if (command.equals("Defined range"))
        {
            view.view.setDisplayRangePolicy(KahinaChartView.RANGE_USED_OR_CAPTION_DEFINED);
        }
        else if (command.equals("Complete range"))
        {
            view.view.setDisplayRangePolicy(KahinaChartView.RANGE_COMPLETE);
        }
        else if (command.equals("Antialiasing On"))
        {
            view.view.setAntialiasingPolicy(KahinaChartView.ANTIALIASING);
        }
        else if (command.equals("Antialiasing Off"))
        {
            view.view.setAntialiasingPolicy(KahinaChartView.NO_ANTIALIASING);
        }
        else if (command.endsWith("0 %"))
        {
            int zoomLevel = Integer.parseInt(command.substring(0, command.length() - 3));
            view.view.setZoomLevel(zoomLevel);
        }
        else if (command.equals("Save as PNG"))
        {
            JFileChooser chooser = new JFileChooser(new File("."));
            //FileNameExtensionFilter filter = new FileNameExtensionFilter("PNG Images", "png");
            //chooser.setFileFilter(filter);
            chooser.showSaveDialog(view);
            File outputFile = chooser.getSelectedFile();
            
            BufferedImage outputImage = new BufferedImage(view.view.getDisplayWidth() + 8, view.view.getDisplayHeight() + view.view.cellHeight + 8, BufferedImage.TYPE_INT_ARGB);
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
        view.view.recalculate();
        view.updateDisplay();
        view.repaint();
    }

}
