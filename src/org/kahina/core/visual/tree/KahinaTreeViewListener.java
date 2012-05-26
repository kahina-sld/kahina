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

import org.kahina.core.KahinaInstance;
import org.kahina.core.gui.event.KahinaSelectionEvent;

public class KahinaTreeViewListener extends MouseAdapter implements ActionListener
{
    protected KahinaTreeViewPanel view;
    protected KahinaTreeViewMarker marker;
    protected MouseEvent lastMouseEvent;
	protected final KahinaInstance<?, ?, ?> kahina;
    
    public KahinaTreeViewListener(KahinaTreeViewPanel view, KahinaInstance<?, ?, ?> kahina)
    {
        this.view = view;
        this.marker = new KahinaTreeViewMarker(view.view.getTreeModel());
        this.kahina = kahina;
        marker.registerTreeView(view);
        this.lastMouseEvent = null;
    }
    
    public KahinaTreeViewListener(KahinaTreeViewPanel view, KahinaTreeViewMarker marker, KahinaInstance<?, ?, ?> kahina)
    {
        this.view = view;
        this.marker = marker;
        this.kahina = kahina;
        marker.registerTreeView(view);
        this.lastMouseEvent = null;
    }
    
    @Override
	public void mouseClicked(MouseEvent e)
    {
        int clickedNode = view.view.nodeAtCoordinates(e.getX(), e.getY());
        if (lastMouseEvent != null && e.getWhen() - lastMouseEvent.getWhen() < 500)
        {
            if (view.view.getConfig().getCollapsePolicy() == KahinaTreeViewOptions.COLLAPSE_SECONDARY)
            {
                view.view.secondaryTreeModel.toggleCollapse(clickedNode);
            }
            else if (view.view.getConfig().getCollapsePolicy() == KahinaTreeViewOptions.COLLAPSE_PRIMARY)
            {
                view.view.getModel().toggleCollapse(clickedNode);
            }
            view.view.recalculate();
            view.updateDisplayAndRepaintFromEventDispatchThread();
        }
        else
        {
            kahina.dispatchEvent(new KahinaSelectionEvent(clickedNode));
            lastMouseEvent = e;
        }
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
            KahinaTreeViewContextMenu.getMenu(this, view.view).show(e.getComponent(),e.getX(), e.getY());
        }
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String command = e.getActionCommand();
        if (command.equals("Zoom In"))
        {
            view.view.getConfig().zoomIn();
            view.view.recalculate();
        }
        else if (command.equals("Zoom Out"))
        {
            view.view.getConfig().zoomOut();
            view.view.recalculate();
        }
        else if (command.equals("Bottom Up"))
        {
            view.view.getConfig().setDisplayOrientation(KahinaTreeViewOptions.BOTTOM_UP_DISPLAY);
            view.view.recalculate();
        }
        else if (command.equals("Top Down"))
        {
            view.view.getConfig().setDisplayOrientation(KahinaTreeViewOptions.TOP_DOWN_DISPLAY);
            view.view.recalculate();
        }
        else if (command.equals("Increase vertical distance"))
        {
            view.view.getConfig().increaseVerticalDistance();
            view.view.recalculate();
        }
        else if (command.equals("Decrease vertical distance"))
        {
            view.view.getConfig().decreaseVerticalDistance();
            view.view.recalculate();
        }
        else if (command.equals("Increase horizontal distance"))
        {
            view.view.getConfig().increaseHorizontalDistance();
            view.view.recalculate();
        }
        else if (command.equals("Decrease horizontal distance"))
        {
            view.view.getConfig().decreaseHorizontalDistance();
            view.view.recalculate();
        }
        else if (command.equals("No special treatment"))
        {
            view.view.getConfig().setTerminalsPolicy(KahinaTreeViewOptions.NO_SPECIAL_TREATMENT);
            view.view.recalculate();
        }
        else if (command.equals("On extra level"))
        {
            view.view.getConfig().setTerminalsPolicy(KahinaTreeViewOptions.ON_EXTRA_LEVEL);
            view.view.recalculate();
        }
        else if (command.equals("Graphically separated"))
        {
            view.view.getConfig().setTerminalsPolicy(KahinaTreeViewOptions.GRAPHICALLY_SEPARATED);
            view.view.recalculate();
        }
        else if (command.equals("Always"))
        {
            view.view.getConfig().setNodeDisplayPolicy(KahinaTreeViewOptions.ALWAYS);
            view.view.recalculate();
        }
        else if (command.equals("Status decides, default: YES"))
        {
            view.view.getConfig().setNodeDisplayPolicy(KahinaTreeViewOptions.STATUS_DEFAULT_YES);
            view.view.recalculate();
        }
        else if (command.equals("Status decides, default: NO"))
        {
            view.view.getConfig().setNodeDisplayPolicy(KahinaTreeViewOptions.STATUS_DEFAULT_NO);
            view.view.recalculate();
        }
        else if (command.equals("Never"))
        {
            view.view.getConfig().setNodeDisplayPolicy(KahinaTreeViewOptions.NEVER);
            view.view.recalculate();
        }
        else if (command.equals("External conditions"))
        {
            view.view.getConfig().setNodeDisplayPolicy(KahinaTreeViewOptions.CONDITIONALLY);
            view.view.recalculate();
        }
        else if (command.equals("No collapsing"))
        {
            view.view.getConfig().setCollapsePolicy(KahinaTreeViewOptions.NO_COLLAPSING);
            view.view.recalculate();
        }
        else if (command.equals("Collapse primary dimension"))
        {
            view.view.getConfig().setCollapsePolicy(KahinaTreeViewOptions.COLLAPSE_PRIMARY);
            view.view.recalculate();
        }
        else if (command.equals("Collapse secondary dimension"))
        {
            view.view.getConfig().setCollapsePolicy(KahinaTreeViewOptions.COLLAPSE_SECONDARY);
            view.view.recalculate();
        }
        else if (command.equals("Point nodes"))
        {
            view.view.getConfig().setNodeShapePolicy(KahinaTreeViewOptions.NODE_SHAPE_POINT);
        }
        else if (command.equals("Box nodes"))
        {
            view.view.getConfig().setNodeShapePolicy(KahinaTreeViewOptions.NODE_SHAPE_BOX);
        }
        else if (command.equals("Oval nodes"))
        {
            view.view.getConfig().setNodeShapePolicy(KahinaTreeViewOptions.NODE_SHAPE_OVAL);
        }
        else if (command.equals("No tags"))
        {
            view.view.getConfig().setEdgeTagPolicy(KahinaTreeViewOptions.NO_EDGE_TAGS);
        }
        else if (command.equals("Simple tags"))
        {
            view.view.getConfig().setEdgeTagPolicy(KahinaTreeViewOptions.SIMPLE_EDGE_TAGS);
        }
        else if (command.equals("Oval tags"))
        {
            view.view.getConfig().setEdgeTagPolicy(KahinaTreeViewOptions.OVAL_EDGE_TAGS);
        }
        else if (command.equals("Boxed tags"))
        {
            view.view.getConfig().setEdgeTagPolicy(KahinaTreeViewOptions.BOXED_EDGE_TAGS);
        }
        else if (command.equals("Direct"))
        {
            view.view.getConfig().setLineShapePolicy(KahinaTreeViewOptions.STRAIGHT_LINES);
        }
        else if (command.equals("Edgy"))
        {
            view.view.getConfig().setLineShapePolicy(KahinaTreeViewOptions.EDGY_LINES);
        }
        else if (command.equals("Invisible"))
        {
            view.view.getConfig().setLineShapePolicy(KahinaTreeViewOptions.INVISIBLE_LINES);
        }
        else if (command.equals("Secondary direct"))
        {
            view.view.getConfig().setSecondaryLineShapePolicy(KahinaTreeViewOptions.STRAIGHT_LINES);
        }
        else if (command.equals("Secondary edgy"))
        {
            view.view.getConfig().setSecondaryLineShapePolicy(KahinaTreeViewOptions.EDGY_LINES);
        }
        else if (command.equals("Secondary invisible"))
        {
            view.view.getConfig().setSecondaryLineShapePolicy(KahinaTreeViewOptions.INVISIBLE_LINES);
        }
        else if (command.equals("Centered"))
        {
            view.view.getConfig().setNodePositionPolicy(KahinaTreeViewOptions.CENTERED_NODES);
            view.view.recalculate();
        }
        else if (command.equals("Left alignment"))
        {
            view.view.getConfig().setNodePositionPolicy(KahinaTreeViewOptions.LEFT_ALIGNED_NODES);
            view.view.recalculate();
        }
        else if (command.equals("Right alignment"))
        {
            view.view.getConfig().setNodePositionPolicy(KahinaTreeViewOptions.RIGHT_ALIGNED_NODES);
            view.view.recalculate();
        }
        else if (command.equals("Antialiasing On"))
        {
            view.view.getConfig().setAntialiasingPolicy(KahinaTreeViewOptions.ANTIALIASING);
        }
        else if (command.equals("Antialiasing Off"))
        {
            view.view.getConfig().setAntialiasingPolicy(KahinaTreeViewOptions.NO_ANTIALIASING);
        }
        else if (command.endsWith("0 %"))
        {
            int zoomLevel = Integer.parseInt(command.substring(0, command.length() - 3));
            view.view.getConfig().setZoomLevel(zoomLevel);
            view.view.recalculate();
        }
        else if (command.endsWith(" vertical distance"))
        {
            int vertDist = Integer.parseInt(command.substring(0, command.length() - 18));
            view.view.getConfig().setVerticalDistance(vertDist);
            view.view.recalculate();
        }
        else if (command.endsWith(" horizontal distance"))
        {
            int horiDist = Integer.parseInt(command.substring(0, command.length() - 20));
            view.view.getConfig().setHorizontalDistance(horiDist);
            view.view.recalculate();
        }
        else if (command.equals("Display second dimension"))
        {
            view.view.getConfig().toggleSecondDimensionDisplay();
            view.view.recalculate();
        }
        else if (command.equals("Swap dimensions"))
        {
            view.view.swapDimensions();
            view.view.recalculate();
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
        view.updateDisplayAndRepaintFromEventDispatchThread();
    }
}
