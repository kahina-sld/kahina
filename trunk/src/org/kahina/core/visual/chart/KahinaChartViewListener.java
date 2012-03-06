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

import org.kahina.core.gui.event.KahinaEdgeSelectionEvent;

public class KahinaChartViewListener extends MouseAdapter implements ActionListener
{
    KahinaChartViewPanel view;
    
    public KahinaChartViewListener(KahinaChartViewPanel view)
    {
        this.view = view;
    }
    
    @Override
	public void mouseClicked(MouseEvent e)
    {
        int clickedEdge = view.view.edgeAtCoordinates(e.getX() - 5, e.getY() - 5);
        //marking and redrawing happens indirectly
        view.view.control.processEvent(new KahinaEdgeSelectionEvent(clickedEdge));
    }
    
    @Override
	public void mousePressed(MouseEvent e) 
    {
        maybeShowPopup(e);
    }

    @Override
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
            view.view.getConfig().zoomIn();
        }
        else if (command.equals("Zoom Out"))
        {
            view.view.getConfig().zoomOut();
        }
        else if (command.equals("Minimal necessary width"))
        {
            view.view.getConfig().setCellWidthPolicy(KahinaChartViewOptions.MINIMAL_NECESSARY_WIDTH);
        }
        else if (command.equals("Maximal necessary width"))
        {
            view.view.getConfig().setCellWidthPolicy(KahinaChartViewOptions.MAXIMAL_NECESSARY_WIDTH);
        }
        else if (command.equals("Fixed width"))
        {
            view.view.getConfig().setCellWidthPolicy(KahinaChartViewOptions.FIXED_WIDTH);
        }
        else if (command.equals("Ancestors and Descendants"))
        {
            view.view.getConfig().setDependencyDisplayPolicy(KahinaChartViewOptions.BOTH_ANCESTORS_AND_DESCENDANTS);
        }
        else if (command.equals("Only ancestors (= production)"))
        {
            view.view.getConfig().setDependencyDisplayPolicy(KahinaChartViewOptions.ANCESTORS_ONLY);
        }
        else if (command.equals("Only descendants (= origin)"))
        {
            view.view.getConfig().setDependencyDisplayPolicy(KahinaChartViewOptions.DESCENDANTS_ONLY);
        }
        else if (command.equals("No dependencies"))
        {
            view.view.getConfig().setDependencyDisplayPolicy(KahinaChartViewOptions.NO_DEPENDENCIES);
        }
        else if (command.equals("ancestorTransitivity"))
        {
            view.view.getConfig().swapAncestorTransitivity();
        }
        else if (command.equals("descendantTransitivity"))
        {
            view.view.getConfig().swapDescendantTransitivity();
        }
        else if (command.equals("Fill space compactly"))
        {
            view.view.getConfig().setEdgeStackingPolicy(KahinaChartViewOptions.STACK_EDGES_FILL_SPACE);
        }
        else if (command.equals("Maintain chronological order"))
        {
            view.view.getConfig().setEdgeStackingPolicy(KahinaChartViewOptions.STACK_EDGES_BY_ID);
        }
        else if (command.equals("Bottom Up"))
        {
            view.view.getConfig().setDisplayOrientation(KahinaChartViewOptions.BOTTOM_UP_DISPLAY);
        }
        else if (command.equals("Top Down"))
        {
            view.view.getConfig().setDisplayOrientation(KahinaChartViewOptions.TOP_DOWN_DISPLAY);
        }
        else if (command.equals("Used range only"))
        {
            view.view.getConfig().setDisplayRangePolicy(KahinaChartViewOptions.RANGE_USED_ONLY);
        }
        else if (command.equals("Defined range"))
        {
            view.view.getConfig().setDisplayRangePolicy(KahinaChartViewOptions.RANGE_USED_OR_CAPTION_DEFINED);
        }
        else if (command.equals("Complete range"))
        {
            view.view.getConfig().setDisplayRangePolicy(KahinaChartViewOptions.RANGE_COMPLETE);
        }
        else if (command.equals("Antialiasing On"))
        {
            view.view.getConfig().setAntialiasingPolicy(KahinaChartViewOptions.ANTIALIASING);
        }
        else if (command.equals("Antialiasing Off"))
        {
            view.view.getConfig().setAntialiasingPolicy(KahinaChartViewOptions.NO_ANTIALIASING);
        }
        else if (command.endsWith("0 %"))
        {
            int zoomLevel = Integer.parseInt(command.substring(0, command.length() - 3));
            view.view.getConfig().setZoomLevel(zoomLevel);
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
        else if (command.startsWith("edgeLabel:"))
        {
            String edgeLabel = command.substring(10);
            view.view.getConfig().displayDecider.swapLabelDisplay(edgeLabel);
        }
        view.view.recalculate();
        view.view.updateHighlightings();
        view.updateDisplayAndRepaintFromEventDispatchThread();
        view.repaint();
    }

}
