package org.kahina.core.visual.graph;

import java.awt.event.ActionListener;

import javax.swing.ButtonGroup;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButtonMenuItem;

public class KahinaGraphViewContextMenu extends JPopupMenu
{
    private static final long serialVersionUID = -6792818227134710399L;
    
    KahinaGraphView v;
    
    public KahinaGraphViewContextMenu(ActionListener l, KahinaGraphView v)
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
        JRadioButtonMenuItem noTagItem = new JRadioButtonMenuItem("No tags");
        noTagItem.addActionListener(l);
        edgeShapePolicyGroup.add(noTagItem);
        edgeShapePolicySubmenu.add(noTagItem);
        JRadioButtonMenuItem simpleTagItem = new JRadioButtonMenuItem("Simple tags");
        simpleTagItem.addActionListener(l);
        edgeShapePolicyGroup.add(simpleTagItem);
        edgeShapePolicySubmenu.add(simpleTagItem);
        JRadioButtonMenuItem ovalTagItem = new JRadioButtonMenuItem("Oval tags");
        ovalTagItem.addActionListener(l);
        edgeShapePolicyGroup.add(ovalTagItem);
        edgeShapePolicySubmenu.add(ovalTagItem);
        JRadioButtonMenuItem boxedTagItem = new JRadioButtonMenuItem("Boxed tags");
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
        
        JMenuItem optimizeItem = new JMenuItem("Optimize");
        optimizeItem.addActionListener(l);
        add(optimizeItem);
        
        JMenuItem optimizeVisibleItem = new JMenuItem("Optimize Visible Vertices");
        optimizeVisibleItem.addActionListener(l);
        add(optimizeVisibleItem);
        
        addSeparator();
        
        JMenuItem exportPNGItem = new JMenuItem("Save as PNG");
        exportPNGItem.addActionListener(l);
        add(exportPNGItem);
    }
    
    public static JPopupMenu getMenu(ActionListener l, KahinaGraphView v)
    {
        return new KahinaGraphViewContextMenu(l, v);
    }
}
