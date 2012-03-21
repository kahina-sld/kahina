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
        else if (command.equals("edgeShapeDirect"))
        {
            view.view.getConfig().setEdgeShapePolicy(KahinaGraphViewOptions.EDGE_SHAPE_DIRECT);
        }
        else if (command.equals("edgeShapeRect"))
        {
            view.view.getConfig().setEdgeShapePolicy(KahinaGraphViewOptions.EDGE_SHAPE_RECTANGULAR);
        }
        else if (command.equals("edgeShapeArc"))
        {
            view.view.getConfig().setEdgeShapePolicy(KahinaGraphViewOptions.EDGE_SHAPE_ARC);
        }
        else if (command.equals("Antialiasing On"))
        {
            view.view.getConfig().setAntialiasingPolicy(KahinaGraphViewOptions.ANTIALIASING);
        }
        else if (command.equals("Antialiasing Off"))
        {
            view.view.getConfig().setAntialiasingPolicy(KahinaGraphViewOptions.NO_ANTIALIASING);
        }
        else if (command.equals("gridLayout"))
        {
            view.view.getConfig().setGraphLayout(KahinaGraphViewOptions.LAYOUT_GRID);
            view.view.setLayouter(new GridLayouter());
        }
        else if (command.equals("circularLayout"))
        {
            view.view.getConfig().setGraphLayout(KahinaGraphViewOptions.LAYOUT_CIRCULAR);
            view.view.setLayouter(new CircularLayouter());
        }
        else if (command.equals("springLayout"))
        {
            view.view.getConfig().setGraphLayout(KahinaGraphViewOptions.LAYOUT_SPRING);
            view.view.setLayouter(new SpringLayouter());
        }
        else if (command.equals("vertexVisAll"))
        {
            view.view.getConfig().setVertexVisibilityPolicy(KahinaGraphViewOptions.VERTICES_ALL_VISIBLE);
        }
        else if (command.equals("vertexVisSpecial"))
        {
            view.view.getConfig().setVertexVisibilityPolicy(KahinaGraphViewOptions.VERTICES_SPECIAL_VISIBLE);
        }
        else if (command.equals("vertexVisExplicit"))
        {
            view.view.getConfig().setVertexVisibilityPolicy(KahinaGraphViewOptions.VERTICES_EXPLICITLY_VISIBLE);
        }
        else if (command.equals("edgeVisAll"))
        {
            view.view.getConfig().setEdgeVisibilityPolicy(KahinaGraphViewOptions.EDGES_ALL_VISIBLE);
        }
        else if (command.equals("edgeVisOneVisible"))
        {
            view.view.getConfig().setEdgeVisibilityPolicy(KahinaGraphViewOptions.EDGES_WITH_ONE_NODE_VISIBLE);
        }
        else if (command.equals("edgeVisBothVisible"))
        {
            view.view.getConfig().setEdgeVisibilityPolicy(KahinaGraphViewOptions.EDGES_WITH_BOTH_NODES_VISIBLE);
        }
        else if (command.equals("edgeColIndependent"))
        {
            view.view.getConfig().setEdgeColoringPolicy(KahinaGraphViewOptions.EDGE_COLOR_INDEPENDENT);
        }
        else if (command.equals("edgeColFunction"))
        {
            view.view.getConfig().setEdgeColoringPolicy(KahinaGraphViewOptions.EDGE_COLOR_FUNCTION_OF_VERTEX_COLOR);
        }
        else if (command.equals("edgeColOnlySameColor"))
        {
            view.view.getConfig().setEdgeColoringPolicy(KahinaGraphViewOptions.EDGE_COLOR_BETWEEN_NODES_OF_SAME_COLOR);
        }
        else if (command.equals("spePosSeparate"))
        {
            view.view.getConfig().setSpecialVertexPositionPolicy(KahinaGraphViewOptions.SPECIAL_VERTICES_SEPARATE);
            view.view.layout.refreshCoordinates();
        }
        else if (command.equals("spePosMixed"))
        {
            view.view.getConfig().setSpecialVertexPositionPolicy(KahinaGraphViewOptions.SPECIAL_VERTICES_MIXED);
            view.view.layout.refreshCoordinates();
        }
        else if (command.equals("speColHighlight"))
        {
            view.view.getConfig().setSpecialVertexColoringPolicy(KahinaGraphViewOptions.SPECIAL_VERTICES_HIGHLIGHTED);
        }
        else if (command.equals("speColNormal"))
        {
            view.view.getConfig().setSpecialVertexColoringPolicy(KahinaGraphViewOptions.SPECIAL_VERTICES_NORMAL_COLOR);
        }
        else if (command.equals("Optimize"))
        {
            String numIterations = (String) JOptionPane.showInputDialog(
                    view,
                    "How many iterations?",
                    "Customized Dialog",
                    JOptionPane.QUESTION_MESSAGE,
                    null,
                    null, "1");
            if ((numIterations != null) && (numIterations.length() > 0)) 
            {
                int it = Integer.parseInt(numIterations);
                for (int i = 0; i < it; i++)
                {
                    view.view.layout.optimize();
                    view.view.layout.refreshCoordinates();
                }
            }
        }
        else if (command.equals("optVisVrtAllEdges"))
        {
            for (int v : view.view.getVisibleVertices())
            {
                view.view.layout.optimizeVtxPosAllEdges(v);
            }
            view.view.layout.refreshCoordinates();
        }
        else if (command.equals("optVisVrtVisEdges"))
        {
            for (int v : view.view.getVisibleVertices())
            {
                view.view.layout.optimizeVtxPosVisibleEdges(v);
            }
            view.view.layout.refreshCoordinates();
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
