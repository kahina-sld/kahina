package org.kahina.core.visual.dag;

import java.awt.event.ActionListener;

import javax.swing.ButtonGroup;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButtonMenuItem;

import org.kahina.core.visual.graph.KahinaGraphViewOptions;


public class KahinaDAGViewContextMenu extends JPopupMenu
{
    private static final long serialVersionUID = 9048500561913489861L;
    
    KahinaDAGView v;
    
    public KahinaDAGViewContextMenu(ActionListener l, KahinaDAGView v)
    {
        super();
        this.v = v;
        JMenu zoomSubmenu = new JMenu("Zoom");
        JMenuItem zoomInItem = new JMenuItem("Zoom In");
        zoomInItem.addActionListener(l);
        zoomSubmenu.add(zoomInItem);
        JMenuItem zoomOutItem = new JMenuItem("Zoom Out");
        zoomOutItem.addActionListener(l);
        zoomSubmenu.add(zoomOutItem);
        zoomSubmenu.addSeparator();
        ButtonGroup sizeGroup = new ButtonGroup();
        //size buttons visible depend on current size
        for (int i = 6; i <= 30; i += 1)
        {
            JRadioButtonMenuItem sizeItem = new JRadioButtonMenuItem(i + "0 %");
            if (i == v.getConfig().getZoomLevel()) sizeItem.setSelected(true);
            sizeItem.addActionListener(l);
            sizeGroup.add(sizeItem);
            zoomSubmenu.add(sizeItem);
        }
        add(zoomSubmenu);
        
        JMenu verticalDistanceSubmenu = new JMenu("Vertical distance");
        JMenuItem increaseVerticalDistanceItem = new JMenuItem("Increase");
        increaseVerticalDistanceItem.setActionCommand("Increase vertical distance");
        increaseVerticalDistanceItem.addActionListener(l);
        verticalDistanceSubmenu.add(increaseVerticalDistanceItem);
        JMenuItem decreaseVerticalDistanceItem = new JMenuItem("Decrease");
        decreaseVerticalDistanceItem.setActionCommand("Decrease vertical distance");
        decreaseVerticalDistanceItem.addActionListener(l);
        verticalDistanceSubmenu.add(decreaseVerticalDistanceItem);
        verticalDistanceSubmenu.addSeparator();
        ButtonGroup verticalDistanceGroup = new ButtonGroup();
        //size buttons visible depend on current size
        for (int i = 2; i <= 20; i += 1)
        {
            JRadioButtonMenuItem distanceItem = new JRadioButtonMenuItem(i + " px");
            distanceItem.setActionCommand(i + " vertical distance");
            if (i == v.getConfig().getVerticalDistance()) distanceItem.setSelected(true);
            distanceItem.addActionListener(l);
            verticalDistanceGroup.add(distanceItem);
            verticalDistanceSubmenu.add(distanceItem);
        }
        add(verticalDistanceSubmenu);
        
        JMenu horizontalDistanceSubmenu = new JMenu("Horizontal distance");
        JMenuItem increasehorizontalDistanceItem = new JMenuItem("Increase");
        increasehorizontalDistanceItem.setActionCommand("Increase horizontal distance");
        increasehorizontalDistanceItem.addActionListener(l);
        horizontalDistanceSubmenu.add(increasehorizontalDistanceItem);
        JMenuItem decreasehorizontalDistanceItem = new JMenuItem("Decrease");
        decreasehorizontalDistanceItem.setActionCommand("Decrease horizontal distance");
        decreasehorizontalDistanceItem.addActionListener(l);
        horizontalDistanceSubmenu.add(decreasehorizontalDistanceItem);
        horizontalDistanceSubmenu.addSeparator();
        ButtonGroup horizontalDistanceGroup = new ButtonGroup();
        //size buttons visible depend on current size
        for (int i = 2; i <= 20; i += 1)
        {
            JRadioButtonMenuItem distanceItem = new JRadioButtonMenuItem(i + " px");
            distanceItem.setActionCommand(i + " horizontal distance");
            if (i == v.getConfig().getHorizontalDistance()) distanceItem.setSelected(true);
            distanceItem.addActionListener(l);
            horizontalDistanceGroup.add(distanceItem);
            horizontalDistanceSubmenu.add(distanceItem);
        }
        add(horizontalDistanceSubmenu);
        
        JMenu nodeSizeSubmenu = new JMenu("Node Size");
        JMenuItem increaseNodeSizeItem = new JMenuItem("Increase Node Size");
        increaseNodeSizeItem.addActionListener(l);
        nodeSizeSubmenu.add(increaseNodeSizeItem);
        JMenuItem decreaseNodeSizeItem = new JMenuItem("Decrease Node Size");
        decreaseNodeSizeItem.addActionListener(l);
        nodeSizeSubmenu.add(decreaseNodeSizeItem);
        nodeSizeSubmenu.addSeparator();
        ButtonGroup nodeSizeGroup = new ButtonGroup();
        //size buttons visible depend on current size
        for (int i = 1; i <= 20; i += 1)
        {
            JRadioButtonMenuItem sizeItem = new JRadioButtonMenuItem(i + " pt");
            if (i == v.getConfig().getNodeSize()) sizeItem.setSelected(true);
            sizeItem.addActionListener(l);
            nodeSizeGroup.add(sizeItem);
            nodeSizeSubmenu.add(sizeItem);
        }
        add(nodeSizeSubmenu);
        
        addSeparator();
        
        JMenu nodeShapePolicySubmenu = new JMenu("Vertex labels");
        ButtonGroup nodeShapePolicyGroup = new ButtonGroup();
        JRadioButtonMenuItem pointShapeItem = new JRadioButtonMenuItem("Point vertices");
        pointShapeItem.addActionListener(l);
        nodeShapePolicyGroup.add(pointShapeItem);
        nodeShapePolicySubmenu.add(pointShapeItem);
        JRadioButtonMenuItem boxShapeItem = new JRadioButtonMenuItem("Box vertices");
        boxShapeItem.addActionListener(l);
        nodeShapePolicyGroup.add(boxShapeItem);
        nodeShapePolicySubmenu.add(boxShapeItem);
        JRadioButtonMenuItem ovalShapeItem = new JRadioButtonMenuItem("Oval vertices");
        ovalShapeItem.addActionListener(l);
        nodeShapePolicyGroup.add(ovalShapeItem);
        nodeShapePolicySubmenu.add(ovalShapeItem);
        switch (v.getConfig().getVertexShapePolicy())
        {
            case KahinaGraphViewOptions.POINT_VERTICES:
            {
                pointShapeItem.setSelected(true); break;
            }
            case KahinaGraphViewOptions.BOX_VERTICES:
            {
                boxShapeItem.setSelected(true); break;
            }
            case KahinaGraphViewOptions.OVAL_VERTICES:
            {
                ovalShapeItem.setSelected(true);
            }
        }
        add(nodeShapePolicySubmenu);
        
        JMenu edgeShapePolicySubmenu = new JMenu("Edge labels");
        ButtonGroup edgeShapePolicyGroup = new ButtonGroup();
        JRadioButtonMenuItem noTagItem = new JRadioButtonMenuItem("No edge labels");
        noTagItem.addActionListener(l);
        edgeShapePolicyGroup.add(noTagItem);
        edgeShapePolicySubmenu.add(noTagItem);
        JRadioButtonMenuItem simpleTagItem = new JRadioButtonMenuItem("Simple edge labels");
        simpleTagItem.addActionListener(l);
        edgeShapePolicyGroup.add(simpleTagItem);
        edgeShapePolicySubmenu.add(simpleTagItem);
        JRadioButtonMenuItem ovalTagItem = new JRadioButtonMenuItem("Oval edge labels");
        ovalTagItem.addActionListener(l);
        edgeShapePolicyGroup.add(ovalTagItem);
        edgeShapePolicySubmenu.add(ovalTagItem);
        JRadioButtonMenuItem boxedTagItem = new JRadioButtonMenuItem("Boxed edge labels");
        boxedTagItem.addActionListener(l);
        edgeShapePolicyGroup.add(boxedTagItem);
        edgeShapePolicySubmenu.add(boxedTagItem);

        switch (v.getConfig().getEdgeLabelPolicy())
        {
            case 0:
            {
                noTagItem.setSelected(true); break;
            }
            case 1:
            {
                simpleTagItem.setSelected(true); break;
            }
            case 2:
            {
                boxShapeItem.setSelected(true); break;
            }
            case 3:
            {
                ovalTagItem.setSelected(true);
            }
        }
        add(edgeShapePolicySubmenu); 
        
        JMenu antialiasingSubmenu = new JMenu("Antialiasing");
        ButtonGroup antialiasingGroup = new ButtonGroup();
        JRadioButtonMenuItem antialiasingOnItem = new JRadioButtonMenuItem("On");
        antialiasingOnItem.setActionCommand("Antialiasing On");
        antialiasingOnItem.addActionListener(l);
        antialiasingGroup.add(antialiasingOnItem);
        antialiasingSubmenu.add(antialiasingOnItem);
        JRadioButtonMenuItem antialiasingOffItem = new JRadioButtonMenuItem("Off");
        antialiasingOffItem.setActionCommand("Antialiasing Off");
        antialiasingOffItem.addActionListener(l);
        antialiasingGroup.add(antialiasingOffItem);
        antialiasingSubmenu.add(antialiasingOffItem);
        switch (v.getConfig().getAntialiasingPolicy())
        {
            case 0:
            {
                antialiasingOnItem.setSelected(true); break;
            }
            case 1:
            {
                antialiasingOffItem.setSelected(true);
            }
        }
        add(antialiasingSubmenu);
        
        addSeparator();
        
        JMenuItem exportPNGItem = new JMenuItem("Save as PNG");
        exportPNGItem.addActionListener(l);
        add(exportPNGItem);
    }
    
    public static JPopupMenu getMenu(ActionListener l, KahinaDAGView v)
    {
        return new KahinaDAGViewContextMenu(l, v);
    }
}
