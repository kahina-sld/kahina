package org.kahina.core.visual.tree;

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

import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.gui.event.KahinaSelectionEvent;

public class KahinaTreeViewListener extends MouseAdapter implements ActionListener
{
    KahinaTreeViewPanel view;
    KahinaTreeViewMarker marker;
    MouseEvent lastMouseEvent;
    
    public KahinaTreeViewListener(KahinaTreeViewPanel view)
    {
        this.view = view;
        this.marker = new KahinaTreeViewMarker((KahinaTree) view.v.getTreeModel());
        marker.registerTreeView(view);
        this.lastMouseEvent = null;
    }
    
    public KahinaTreeViewListener(KahinaTreeViewPanel view, KahinaTreeViewMarker marker)
    {
        this.view = view;
        this.marker = marker;
        marker.registerTreeView(view);
        this.lastMouseEvent = null;
    }
    
    public void mouseClicked(MouseEvent e)
    {
        int clickedNode = view.v.nodeAtCoordinates(e.getX(), e.getY());
        if (lastMouseEvent != null && e.getWhen() - lastMouseEvent.getWhen() < 500)
        {
            if (view.v.getCollapsePolicy() == KahinaTreeView.COLLAPSE_SECONDARY)
            {
                view.v.secondaryTreeModel.toggleCollapse(clickedNode);
            }
            else if (view.v.getCollapsePolicy() == KahinaTreeView.COLLAPSE_PRIMARY)
            {
                view.v.getModel().toggleCollapse(clickedNode);
            }
            view.v.recalculate();
            view.updateDisplay();
            view.repaint();
        }
        else
        {
            marker.markNode(clickedNode);
            lastMouseEvent = e;
        }
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
            KahinaTreeViewContextMenu.getMenu(this, view.v).show(e.getComponent(),e.getX(), e.getY());
        }
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String command = e.getActionCommand();
        if (command.equals("Zoom In"))
        {
            view.v.zoomIn();
            view.v.recalculate();
        }
        else if (command.equals("Zoom Out"))
        {
            view.v.zoomOut();
            view.v.recalculate();
        }
        else if (command.equals("Bottom Up"))
        {
            view.v.setDisplayOrientation(KahinaTreeView.BOTTOM_UP_DISPLAY);
            view.v.recalculate();
        }
        else if (command.equals("Top Down"))
        {
            view.v.setDisplayOrientation(KahinaTreeView.TOP_DOWN_DISPLAY);
            view.v.recalculate();
        }
        else if (command.equals("Increase vertical distance"))
        {
            view.v.increaseVerticalDistance();
            view.v.recalculate();
        }
        else if (command.equals("Decrease vertical distance"))
        {
            view.v.decreaseVerticalDistance();
            view.v.recalculate();
        }
        else if (command.equals("Increase horizontal distance"))
        {
            view.v.increaseHorizontalDistance();
            view.v.recalculate();
        }
        else if (command.equals("Decrease horizontal distance"))
        {
            view.v.decreaseHorizontalDistance();
            view.v.recalculate();
        }
        else if (command.equals("No special treatment"))
        {
            view.v.setTerminalsPolicy(KahinaTreeView.NO_SPECIAL_TREATMENT);
            view.v.recalculate();
        }
        else if (command.equals("On extra level"))
        {
            view.v.setTerminalsPolicy(KahinaTreeView.ON_EXTRA_LEVEL);
            view.v.recalculate();
        }
        else if (command.equals("Graphically separated"))
        {
            view.v.setTerminalsPolicy(KahinaTreeView.GRAPHICALLY_SEPARATED);
            view.v.recalculate();
        }
        else if (command.equals("Always"))
        {
            view.v.setNodeDisplayPolicy(KahinaTreeView.ALWAYS);
            view.v.recalculate();
        }
        else if (command.equals("Status decides, default: YES"))
        {
            view.v.setNodeDisplayPolicy(KahinaTreeView.STATUS_DEFAULT_YES);
            view.v.recalculate();
        }
        else if (command.equals("Status decides, default: NO"))
        {
            view.v.setNodeDisplayPolicy(KahinaTreeView.STATUS_DEFAULT_NO);
            view.v.recalculate();
        }
        else if (command.equals("Never"))
        {
            view.v.setNodeDisplayPolicy(KahinaTreeView.NEVER);
            view.v.recalculate();
        }
        else if (command.equals("External conditions"))
        {
            view.v.setNodeDisplayPolicy(KahinaTreeView.CONDITIONALLY);
            view.v.recalculate();
        }
        else if (command.equals("No collapsing"))
        {
            view.v.setCollapsePolicy(KahinaTreeView.NO_COLLAPSING);
            view.v.recalculate();
        }
        else if (command.equals("Collapse primary dimension"))
        {
            view.v.setCollapsePolicy(KahinaTreeView.COLLAPSE_PRIMARY);
            view.v.recalculate();
        }
        else if (command.equals("Collapse secondary dimension"))
        {
            view.v.setCollapsePolicy(KahinaTreeView.COLLAPSE_SECONDARY);
            view.v.recalculate();
        }
        else if (command.equals("Box nodes"))
        {
            view.v.setNodeShapePolicy(KahinaTreeView.BOX_SHAPE);
        }
        else if (command.equals("Oval nodes"))
        {
            view.v.setNodeShapePolicy(KahinaTreeView.OVAL_SHAPE);
        }
        else if (command.equals("Boxed edge labels"))
        {
            view.v.setEdgeShapePolicy(KahinaTreeView.BOX_SHAPE);
        }
        else if (command.equals("Oval edge labels"))
        {
            view.v.setEdgeShapePolicy(KahinaTreeView.OVAL_SHAPE);
        }
        else if (command.equals("Direct"))
        {
            view.v.setLineShapePolicy(KahinaTreeView.STRAIGHT_LINES);
        }
        else if (command.equals("Edgy"))
        {
            view.v.setLineShapePolicy(KahinaTreeView.EDGY_LINES);
        }
        else if (command.equals("Invisible"))
        {
            view.v.setLineShapePolicy(KahinaTreeView.INVISIBLE_LINES);
        }
        else if (command.equals("Secondary direct"))
        {
            view.v.setSecondaryLineShapePolicy(KahinaTreeView.STRAIGHT_LINES);
        }
        else if (command.equals("Secondary edgy"))
        {
            view.v.setSecondaryLineShapePolicy(KahinaTreeView.EDGY_LINES);
        }
        else if (command.equals("Secondary invisible"))
        {
            view.v.setSecondaryLineShapePolicy(KahinaTreeView.INVISIBLE_LINES);
        }
        else if (command.equals("Centered"))
        {
            view.v.setNodePositionPolicy(KahinaTreeView.CENTERED_NODES);
            view.v.recalculate();
        }
        else if (command.equals("Left alignment"))
        {
            view.v.setNodePositionPolicy(KahinaTreeView.LEFT_ALIGNED_NODES);
            view.v.recalculate();
        }
        else if (command.equals("Right alignment"))
        {
            view.v.setNodePositionPolicy(KahinaTreeView.RIGHT_ALIGNED_NODES);
            view.v.recalculate();
        }
        else if (command.equals("Antialiasing On"))
        {
            view.v.setAntialiasingPolicy(KahinaTreeView.ANTIALIASING);
        }
        else if (command.equals("Antialiasing Off"))
        {
            view.v.setAntialiasingPolicy(KahinaTreeView.NO_ANTIALIASING);
        }
        else if (command.endsWith("0 %"))
        {
            int zoomLevel = Integer.parseInt(command.substring(0, command.length() - 3));
            view.v.setZoomLevel(zoomLevel);
            view.v.recalculate();
        }
        else if (command.endsWith(" vertical distance"))
        {
            int vertDist = Integer.parseInt(command.substring(0, command.length() - 18));
            view.v.setVerticalDistance(vertDist);
            view.v.recalculate();
        }
        else if (command.endsWith(" horizontal distance"))
        {
            int horiDist = Integer.parseInt(command.substring(0, command.length() - 20));
            view.v.setHorizontalDistance(horiDist);
            view.v.recalculate();
        }
        else if (command.equals("Display second dimension"))
        {
            view.v.toggleSecondDimensionDisplay();
            view.v.recalculate();
        }
        else if (command.equals("Swap dimensions"))
        {
            view.v.swapDimensions();
            view.v.recalculate();
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
        view.updateDisplay();
        view.repaint();
    }
}
