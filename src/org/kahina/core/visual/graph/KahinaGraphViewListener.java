package org.kahina.core.visual.graph;

import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import org.kahina.core.KahinaRunner;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.visual.tree.KahinaTreeViewContextMenu;
import org.kahina.core.visual.tree.KahinaTreeViewOptions;

public class KahinaGraphViewListener extends MouseAdapter implements ActionListener
{
    protected KahinaGraphViewPanel view;
    MouseEvent lastMouseEvent;
    
    public KahinaGraphViewListener(KahinaGraphViewPanel view)
    {
        this.view = view;
        this.lastMouseEvent = null;
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

    protected void maybeShowPopup(MouseEvent e) 
    {
        if (e.isPopupTrigger()) 
        {
            KahinaGraphViewContextMenu.getMenu(this, view.view).show(e.getComponent(),e.getX(), e.getY());
        }
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String command = e.getActionCommand();
        if (command.equals("Zoom In"))
        {
            view.view.getConfig().zoomIn();
            view.view.layout.refreshCoordinates();
        }
        else if (command.equals("Zoom Out"))
        {
            view.view.getConfig().zoomOut();
            view.view.layout.refreshCoordinates();
        }
        else if (command.equals("Increase Node Size"))
        {
            view.view.getConfig().increaseNodeSize();
        }
        else if (command.equals("Decrease Node Size"))
        {
            view.view.getConfig().decreaseNodeSize();
        }
        else if (command.equals("Point vertices"))
        {
            view.view.getConfig().setVertexShapePolicy(KahinaGraphViewOptions.POINT_VERTICES);
        }
        else if (command.equals("Box vertices"))
        {
            view.view.getConfig().setVertexShapePolicy(KahinaGraphViewOptions.BOX_VERTICES);
        }
        else if (command.equals("Oval vertices"))
        {
            view.view.getConfig().setVertexShapePolicy(KahinaGraphViewOptions.OVAL_VERTICES);
        }
        else if (command.equals("No edge labels"))
        {
            view.view.getConfig().setEdgeLabelPolicy(KahinaGraphViewOptions.NO_EDGE_LABELS);
        }
        else if (command.equals("Simple edge labels"))
        {
            view.view.getConfig().setEdgeLabelPolicy(KahinaGraphViewOptions.SIMPLE_EDGE_LABELS);
        }
        else if (command.equals("Oval edge labels"))
        {
            view.view.getConfig().setEdgeLabelPolicy(KahinaGraphViewOptions.OVAL_EDGE_LABELS);
        }
        else if (command.equals("Boxed edge labels"))
        {
            view.view.getConfig().setEdgeLabelPolicy(KahinaGraphViewOptions.BOXED_EDGE_LABELS);
        }
        else if (command.equals("verticesAboveEdges"))
        {
            view.view.getConfig().setDrawingOrderPolicy(KahinaGraphViewOptions.VERTICES_ABOVE_EDGES);
        }
        else if (command.equals("edgesAboveVertices"))
        {
            view.view.getConfig().setDrawingOrderPolicy(KahinaGraphViewOptions.EDGES_ABOVE_VERTICES);
        }
        else if (command.equals("Antialiasing On"))
        {
            view.view.getConfig().setAntialiasingPolicy(KahinaGraphViewOptions.ANTIALIASING);
        }
        else if (command.equals("Antialiasing Off"))
        {
            view.view.getConfig().setAntialiasingPolicy(KahinaGraphViewOptions.NO_ANTIALIASING);
        }
        else if (command.equals("Optimize"))
        {
            view.view.layout.optimize();
        }
        else if (command.equals("optVisVrtAllEdges"))
        {
            for (int v : view.view.visibleVertices)
            {
                view.view.layout.optimizeVtxPosAllEdges(v);
            }
        }
        else if (command.equals("optVisVrtVisEdges"))
        {
            for (int v : view.view.visibleVertices)
            {
                view.view.layout.optimizeVtxPosVisibleEdges(v);
            }
        }
        else if (command.endsWith("0 %"))
        {
            int zoomLevel = Integer.parseInt(command.substring(0, command.length() - 3));
            view.view.getConfig().setZoomLevel(zoomLevel);
            view.view.layout.refreshCoordinates();
        }
        else if (command.endsWith(" pt"))
        {
            int nodeSize = Integer.parseInt(command.substring(0, command.length() - 3));
            view.view.getConfig().setNodeSize(nodeSize);
        }
        else if (command.equals("Save as PNG"))
        {
            JFileChooser chooser = new JFileChooser(new File("."));
            //FileNameExtensionFilter filter = new FileNameExtensionFilter("PNG Images", "png");
            //chooser.setFileFilter(filter);
            chooser.showSaveDialog(view);
            File outputFile = chooser.getSelectedFile();

            Graphics outputCanvas = view.image.getGraphics();
            view.paint(outputCanvas);
            try
            {
                ImageIO.write(view.image,"png",outputFile);
            }
            catch (IOException ioe)
            {
                JOptionPane.showMessageDialog(view, ioe.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
            }
        }
        else
        {
            processExtensionCommand(command);
        }
        view.view.flushRedrawAgenda();
        view.updateDisplayAndRepaintFromEventDispatchThread();
    }
    
    //deriving classes can implement this to add functionality for additional control elements
    protected void processExtensionCommand(String command)
    {
        
    }
}
