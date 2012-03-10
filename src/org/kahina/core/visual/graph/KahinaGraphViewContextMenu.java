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
        for (int i = 1; i <= 50; i += 1)
        {
            JRadioButtonMenuItem sizeItem = new JRadioButtonMenuItem(i + "0 %");
            if (i == v.getConfig().getZoomLevel()) sizeItem.setSelected(true);
            sizeItem.addActionListener(l);
            sizeGroup.add(sizeItem);
            zoomSubmenu.add(sizeItem);
        }
        add(zoomSubmenu);
        
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
        
        JMenu drawingOrderSubmenu = new JMenu("Drawing Order");
        ButtonGroup drawingOrderGroup = new ButtonGroup();
        JRadioButtonMenuItem nodesAboveEdgesItem = new JRadioButtonMenuItem("Vertices above Edges");
        nodesAboveEdgesItem.setActionCommand("verticesAboveEdges");
        nodesAboveEdgesItem.addActionListener(l);
        drawingOrderGroup.add(nodesAboveEdgesItem);
        drawingOrderSubmenu.add(nodesAboveEdgesItem);
        JRadioButtonMenuItem edgesAboveNodesItem = new JRadioButtonMenuItem("Edges above Vertices");
        edgesAboveNodesItem.setActionCommand("edgesAboveVertices");
        edgesAboveNodesItem.addActionListener(l);
        drawingOrderGroup.add(edgesAboveNodesItem);
        drawingOrderSubmenu.add(edgesAboveNodesItem);
        switch (v.getConfig().getDrawingOrderPolicy())
        {
            case 0:
            {
                nodesAboveEdgesItem.setSelected(true); break;
            }
            case 1:
            {
                edgesAboveNodesItem.setSelected(true);
            }
        }
        add(drawingOrderSubmenu);
        
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
        
        JMenu graphLayoutSubmenu = new JMenu("Graph Layout");
        ButtonGroup graphLayoutGroup = new ButtonGroup();
        JRadioButtonMenuItem gridLayoutItem = new JRadioButtonMenuItem("Grid Layout");
        gridLayoutItem.setActionCommand("gridLayout");
        gridLayoutItem.addActionListener(l);
        graphLayoutGroup.add(gridLayoutItem);
        graphLayoutSubmenu.add(gridLayoutItem);
        JRadioButtonMenuItem circularLayoutItem = new JRadioButtonMenuItem("Circular Layout");
        circularLayoutItem.setActionCommand("circularLayout");
        circularLayoutItem.addActionListener(l);
        graphLayoutGroup.add(circularLayoutItem);
        graphLayoutSubmenu.add(circularLayoutItem);
        switch (v.getConfig().getGraphLayout())
        {
            case 0:
            {
                gridLayoutItem.setSelected(true); break;
            }
            case 1:
            {
                circularLayoutItem.setSelected(true);
            }
        }
        add(graphLayoutSubmenu);
        
        JMenu nodeVisibilitySubmenu = new JMenu("Vertex Visibility");
        ButtonGroup nodeVisibilityGroup = new ButtonGroup();
        JRadioButtonMenuItem allNodesVisibleItem = new JRadioButtonMenuItem("All Vertices Visible");
        allNodesVisibleItem.setActionCommand("vertexVisAll");
        allNodesVisibleItem.addActionListener(l);
        nodeVisibilityGroup.add(allNodesVisibleItem);
        nodeVisibilitySubmenu.add(allNodesVisibleItem);
        JRadioButtonMenuItem onlySpecialNodesItem = new JRadioButtonMenuItem("Only Special Vertices");
        onlySpecialNodesItem.setActionCommand("vertexVisSpecial");
        onlySpecialNodesItem.addActionListener(l);
        nodeVisibilityGroup.add(onlySpecialNodesItem);
        nodeVisibilitySubmenu.add(onlySpecialNodesItem);
        JRadioButtonMenuItem onlyExplicitlyVisibleNodesItem = new JRadioButtonMenuItem("Only Explicitly Visible Vertices");
        onlyExplicitlyVisibleNodesItem.setActionCommand("vertexVisExplicit");
        onlyExplicitlyVisibleNodesItem.addActionListener(l);
        nodeVisibilityGroup.add(onlyExplicitlyVisibleNodesItem);
        nodeVisibilitySubmenu.add(onlyExplicitlyVisibleNodesItem);
        switch (v.getConfig().getVertexVisibilityPolicy())
        {
            case 0:
            {
                allNodesVisibleItem.setSelected(true); break;
            }
            case 1:
            {
                onlySpecialNodesItem.setSelected(true); break;
            }
            case 2:
            {
                onlyExplicitlyVisibleNodesItem.setSelected(true);
            }
        }
        add(nodeVisibilitySubmenu);
        
        JMenu edgeVisibilitySubmenu = new JMenu("Edge Visibility");
        ButtonGroup edgeVisibilityGroup = new ButtonGroup();
        JRadioButtonMenuItem allEdgesVisibleItem = new JRadioButtonMenuItem("All Edges");
        allEdgesVisibleItem.setActionCommand("edgeVisAll");
        allEdgesVisibleItem.addActionListener(l);
        edgeVisibilityGroup.add(allEdgesVisibleItem);
        edgeVisibilitySubmenu.add(allEdgesVisibleItem);
        JRadioButtonMenuItem onlyEdgesOneVisibleItem = new JRadioButtonMenuItem("Only Edges With Visible Node");
        onlyEdgesOneVisibleItem.setActionCommand("edgeVisOneVisible");
        onlyEdgesOneVisibleItem.addActionListener(l);
        edgeVisibilityGroup.add(onlyEdgesOneVisibleItem);
        edgeVisibilitySubmenu.add(onlyEdgesOneVisibleItem);
        JRadioButtonMenuItem onlyEdgesBothVisibleItem = new JRadioButtonMenuItem("Only Edges Between Visible Nodes");
        onlyEdgesBothVisibleItem.setActionCommand("edgeVisBothVisible");
        onlyEdgesBothVisibleItem.addActionListener(l);
        edgeVisibilityGroup.add(onlyEdgesBothVisibleItem);
        edgeVisibilitySubmenu.add(onlyEdgesBothVisibleItem);
        switch (v.getConfig().getEdgeVisibilityPolicy())
        {
            case 0:
            {
                allEdgesVisibleItem.setSelected(true); break;
            }
            case 1:
            {
                onlyEdgesOneVisibleItem.setSelected(true); break;
            }
            case 2:
            {
                onlyEdgesBothVisibleItem.setSelected(true);
            }
        }
        add(edgeVisibilitySubmenu);
        
        JMenu edgeColoringSubmenu = new JMenu("Edge Coloring");
        ButtonGroup edgeColoringGroup = new ButtonGroup();
        JRadioButtonMenuItem independentColorItem = new JRadioButtonMenuItem("Independent");
        independentColorItem.setActionCommand("edgeColIndependent");
        independentColorItem.addActionListener(l);
        edgeColoringGroup.add(independentColorItem);
        edgeColoringSubmenu.add(independentColorItem);
        JRadioButtonMenuItem functionVertexColorItem = new JRadioButtonMenuItem("Defined Function of Vertex Colors");
        functionVertexColorItem.setActionCommand("edgeColFunction");
        functionVertexColorItem.addActionListener(l);
        edgeColoringGroup.add(functionVertexColorItem);
        edgeColoringSubmenu.add(functionVertexColorItem);
        JRadioButtonMenuItem onlySameColorItem = new JRadioButtonMenuItem("Only Between Nodes of Same Color");
        onlySameColorItem.setActionCommand("edgeColOnlySameColor");
        onlySameColorItem.addActionListener(l);
        edgeColoringGroup.add(onlySameColorItem);
        edgeColoringSubmenu.add(onlySameColorItem);
        switch (v.getConfig().getEdgeColoringPolicy())
        {
            case 0:
            {
                independentColorItem.setSelected(true); break;
            }
            case 1:
            {
               functionVertexColorItem.setSelected(true); break;
            }
            case 2:
            {
                onlySameColorItem.setSelected(true);
            }
        }
        add(edgeColoringSubmenu);
        
        JMenu specialVerticesSubmenu = new JMenu("Special Vertices");
        ButtonGroup specialVertexPositionGroup = new ButtonGroup();
        JRadioButtonMenuItem spePosSeparateItem = new JRadioButtonMenuItem("Spatially Separated");
        spePosSeparateItem.setActionCommand("spePosSeparate");
        spePosSeparateItem.addActionListener(l);
        specialVertexPositionGroup.add(spePosSeparateItem);
        specialVerticesSubmenu.add(spePosSeparateItem);
        JRadioButtonMenuItem spePosMixedItem = new JRadioButtonMenuItem("Mixed With Other Vertices");
        spePosMixedItem.setActionCommand("spePosMixed");
        spePosMixedItem.addActionListener(l);
        specialVertexPositionGroup.add(spePosMixedItem);
        specialVerticesSubmenu.add(spePosMixedItem);
        switch (v.getConfig().getSpecialVertexPositionPolicy())
        {
            case 0:
            {
                spePosSeparateItem.setSelected(true); break;
            }
            case 1:
            {
                spePosMixedItem.setSelected(true);
            }
        }
        specialVerticesSubmenu.addSeparator();
        ButtonGroup specialVertexColoringGroup = new ButtonGroup();
        JRadioButtonMenuItem speColHighlightItem = new JRadioButtonMenuItem("Highlighted, Other Vertices Greyed Out");
        speColHighlightItem.setActionCommand("speColHighlight");
        speColHighlightItem.addActionListener(l);
        specialVertexColoringGroup.add(speColHighlightItem);
        specialVerticesSubmenu.add(speColHighlightItem);
        JRadioButtonMenuItem speColNormalItem = new JRadioButtonMenuItem("No Special Coloring");
        speColNormalItem.setActionCommand("speColNormal");
        speColNormalItem.addActionListener(l);
        specialVertexColoringGroup.add(speColNormalItem);
        specialVerticesSubmenu.add(speColNormalItem);
        switch (v.getConfig().getSpecialVertexColoringPolicy())
        {
            case 0:
            {
                speColHighlightItem.setSelected(true); break;
            }
            case 1:
            {
                speColNormalItem.setSelected(true);
            }
        }
        add(specialVerticesSubmenu);
        
        addSeparator();
        
        JMenuItem optimizeItem = new JMenuItem("Optimize");
        optimizeItem.addActionListener(l);
        add(optimizeItem);
        
        JMenuItem optimizeVisibleItem = new JMenuItem("Optimize Visible Vertices (all edges)");
        optimizeVisibleItem.setActionCommand("optVisVrtAllEdges");
        optimizeVisibleItem.addActionListener(l);
        add(optimizeVisibleItem);
        
        JMenuItem optimizeVisibleEdgesItem = new JMenuItem("Optimize Visible Vertices (visible edges)");
        optimizeVisibleEdgesItem.setActionCommand("optVisVrtVisEdges");
        optimizeVisibleEdgesItem.addActionListener(l);
        add(optimizeVisibleEdgesItem);
        
        addSeparator();
        
        addAdditionalMenus(l);
        
        JMenuItem exportPNGItem = new JMenuItem("Save as PNG");
        exportPNGItem.addActionListener(l);
        add(exportPNGItem);
    }
    
    public static JPopupMenu getMenu(ActionListener l, KahinaGraphView v)
    {
        return new KahinaGraphViewContextMenu(l, v);
    }
    
    //deriving classes can implement this to extend the context menu
    protected void addAdditionalMenus(ActionListener l)
    {
        
    }
}
