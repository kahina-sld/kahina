package org.kahina.core.visual.dag;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.visual.tree.KahinaTreeViewOptions;

public class KahinaDAGViewListener extends MouseAdapter implements ActionListener
{
    KahinaDAGViewPanel view;
    MouseEvent lastMouseEvent;
    
    public KahinaDAGViewListener(KahinaDAGViewPanel view)
    {
        this.view = view;
        this.lastMouseEvent = null;
    }
    
    @Override
	public void mouseClicked(MouseEvent e)
    {
        int clickedNode = view.view.nodeAtCoordinates(e.getX(), e.getY());
        if (lastMouseEvent != null && e.getWhen() - lastMouseEvent.getWhen() < 500)
        {
            view.view.getModel().toggleCollapse(clickedNode);
            view.view.recalculate();
            view.updateDisplayAndRepaintFromEventDispatchThread();
            view.repaint();
        }
        else
        {
            view.view.control.processEvent(new KahinaSelectionEvent(clickedNode));
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
            KahinaDAGViewContextMenu.getMenu(this, view.view).show(e.getComponent(),e.getX(), e.getY());
        }
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String command = e.getActionCommand();
        if (command.equals("Zoom In"))
        {
            view.view.zoomIn();
            view.view.recalculate();
        }
        else if (command.equals("Zoom Out"))
        {
            view.view.zoomOut();
            view.view.recalculate();
        }
        else if (command.equals("Increase vertical distance"))
        {
            view.view.increaseVerticalDistance();
            view.view.recalculate();
        }
        else if (command.equals("Decrease vertical distance"))
        {
            view.view.decreaseVerticalDistance();
            view.view.recalculate();
        }
        else if (command.equals("Increase horizontal distance"))
        {
            view.view.increaseHorizontalDistance();
            view.view.recalculate();
        }
        else if (command.equals("Decrease horizontal distance"))
        {
            view.view.decreaseHorizontalDistance();
            view.view.recalculate();
        }
        else if (command.equals("Antialiasing On"))
        {
            view.view.setAntialiasingPolicy(KahinaTreeViewOptions.ANTIALIASING);
        }
        else if (command.equals("Antialiasing Off"))
        {
            view.view.setAntialiasingPolicy(KahinaTreeViewOptions.NO_ANTIALIASING);
        }
        else if (command.endsWith("0 %"))
        {
            int zoomLevel = Integer.parseInt(command.substring(0, command.length() - 3));
            view.view.setZoomLevel(zoomLevel);
            view.view.recalculate();
        }
        else if (command.endsWith(" vertical distance"))
        {
            int vertDist = Integer.parseInt(command.substring(0, command.length() - 18));
            view.view.setVerticalDistance(vertDist);
            view.view.recalculate();
        }
        else if (command.endsWith(" horizontal distance"))
        {
            int horiDist = Integer.parseInt(command.substring(0, command.length() - 20));
            view.view.setHorizontalDistance(horiDist);
            view.view.recalculate();
        }
        else if (command.equals("Save as PNG"))
        {
            JFileChooser chooser = new JFileChooser(new File("."));
            //FileNameExtensionFilter filter = new FileNameExtensionFilter("PNG Images", "png");
            //chooser.setFileFilter(filter);
            chooser.showSaveDialog(view);
            File outputFile = chooser.getSelectedFile();

            //Graphics outputCanvas = view.image.getGraphics();
            //view.paintComponent(outputCanvas);
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
