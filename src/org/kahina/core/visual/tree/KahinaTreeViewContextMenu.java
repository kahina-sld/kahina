package org.kahina.core.visual.tree;

import java.awt.event.ActionListener;

import javax.swing.ButtonGroup;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButtonMenuItem;

public class KahinaTreeViewContextMenu extends JPopupMenu
{
	private static final long serialVersionUID = 9048500561913489861L;
	
	KahinaTreeView v;
    
    public KahinaTreeViewContextMenu(ActionListener l, KahinaTreeView v)
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
        add(zoomSubmenu);
        
        JMenu nodeSizeSubmenu = new JMenu("Node Size");
        ButtonGroup sizeGroup = new ButtonGroup();
        //size buttons visible depend on current size
        for (int i = 1; i <= 20; i += 1)
        {
            JRadioButtonMenuItem sizeItem = new JRadioButtonMenuItem(i + " pt");
            if (i == v.getConfig().getNodeSize()) sizeItem.setSelected(true);
            sizeItem.addActionListener(l);
            sizeGroup.add(sizeItem);
            nodeSizeSubmenu.add(sizeItem);
        }
        add(nodeSizeSubmenu);
        
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
        for (int i = 1; i <= 20; i += 1)
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
        for (int i = 1; i <= 20; i += 1)
        {
            JRadioButtonMenuItem distanceItem = new JRadioButtonMenuItem(i + " px");
            distanceItem.setActionCommand(i + " horizontal distance");
            if (i == v.getConfig().getHorizontalDistance()) distanceItem.setSelected(true);
            distanceItem.addActionListener(l);
            horizontalDistanceGroup.add(distanceItem);
            horizontalDistanceSubmenu.add(distanceItem);
        }
        add(horizontalDistanceSubmenu);
        
        addSeparator();
        
        JMenu displayOrientationSubmenu = new JMenu("Display orientation");
        ButtonGroup displayOrientationGroup = new ButtonGroup();
        JRadioButtonMenuItem topDownItem = new JRadioButtonMenuItem("Top Down");
        topDownItem.addActionListener(l);
        displayOrientationGroup.add(topDownItem);
        displayOrientationSubmenu.add(topDownItem);
        JRadioButtonMenuItem bottomUpItem = new JRadioButtonMenuItem("Bottom Up");
        bottomUpItem.addActionListener(l);
        displayOrientationGroup.add(bottomUpItem);
        displayOrientationSubmenu.add(bottomUpItem);
        switch (v.getConfig().getDisplayOrientation())
        {
            case 0:
            {
                topDownItem.setSelected(true); break;
            }
            case 1:
            {
                bottomUpItem.setSelected(true);
            }
        }
        add(displayOrientationSubmenu);
        
        JMenu terminalsPolicySubmenu = new JMenu("Terminals");
        ButtonGroup terminalsPolicyGroup = new ButtonGroup();
        JRadioButtonMenuItem noSpecialItem = new JRadioButtonMenuItem("No special treatment");
        noSpecialItem.addActionListener(l);
        terminalsPolicyGroup.add(noSpecialItem);
        terminalsPolicySubmenu.add(noSpecialItem);
        JRadioButtonMenuItem extraLevelItem = new JRadioButtonMenuItem("On extra level");
        extraLevelItem.addActionListener(l);
        terminalsPolicyGroup.add(extraLevelItem);
        terminalsPolicySubmenu.add(extraLevelItem);
        JRadioButtonMenuItem graphicallySeparatedItem = new JRadioButtonMenuItem("Graphically separated");
        graphicallySeparatedItem.addActionListener(l);
        terminalsPolicyGroup.add(graphicallySeparatedItem);
        terminalsPolicySubmenu.add(graphicallySeparatedItem);
        switch (v.getConfig().getTerminalsPolicy())
        {
            case 0:
            {
                noSpecialItem.setSelected(true); break;
            }
            case 1:
            {
                extraLevelItem.setSelected(true); break;
            }
            case 2:
            {
                graphicallySeparatedItem.setSelected(true);
            }
        }
        add(terminalsPolicySubmenu);
        
        JMenu nodeDisplayPolicySubmenu = new JMenu("Node display policy");
        nodeDisplayPolicySubmenu.setEnabled(false);
        ButtonGroup nodeDisplayPolicyGroup = new ButtonGroup();
        JRadioButtonMenuItem alwaysItem = new JRadioButtonMenuItem("Always");
        alwaysItem.addActionListener(l);
        nodeDisplayPolicyGroup.add(alwaysItem);
        nodeDisplayPolicySubmenu.add(alwaysItem);
        JRadioButtonMenuItem statusYesItem = new JRadioButtonMenuItem("Status decides, default: YES");
        statusYesItem.addActionListener(l);
        nodeDisplayPolicyGroup.add(statusYesItem);
        nodeDisplayPolicySubmenu.add(statusYesItem);
        JRadioButtonMenuItem statusNoItem = new JRadioButtonMenuItem("Status decides, default: NO");
        statusNoItem.addActionListener(l);
        nodeDisplayPolicyGroup.add(statusNoItem);
        nodeDisplayPolicySubmenu.add(statusNoItem);
        JRadioButtonMenuItem neverItem = new JRadioButtonMenuItem("Never");
        neverItem.addActionListener(l);
        nodeDisplayPolicyGroup.add(neverItem);
        nodeDisplayPolicySubmenu.add(neverItem);
        nodeDisplayPolicySubmenu.addSeparator();
        JRadioButtonMenuItem conditionalItem = new JRadioButtonMenuItem("External conditions");
        conditionalItem.addActionListener(l);
        nodeDisplayPolicyGroup.add(conditionalItem);
        nodeDisplayPolicySubmenu.add(conditionalItem);
        switch (v.getConfig().getNodeDisplayPolicy())
        {
            case 0:
            {
                alwaysItem.setSelected(true); break;
            }
            case 1:
            {
                statusYesItem.setSelected(true); break;
            }
            case 2:
            {
                statusNoItem.setSelected(true); break;
            }
            case 3:
            {
                neverItem.setSelected(true); break;
            }
            case 4:
            {
                conditionalItem.setSelected(true);
            }
        }
        add(nodeDisplayPolicySubmenu);
        
        JMenu collapsePolicySubmenu = new JMenu("Collapsing");
        ButtonGroup collapsePolicyGroup = new ButtonGroup();
        JRadioButtonMenuItem staticItem = new JRadioButtonMenuItem("No collapsing");
        staticItem.addActionListener(l);
        collapsePolicyGroup.add(staticItem);
        collapsePolicySubmenu.add(staticItem);
        JRadioButtonMenuItem collapsiblePrimaryItem = new JRadioButtonMenuItem("Collapse primary dimension");
        collapsiblePrimaryItem.addActionListener(l);
        collapsePolicyGroup.add(collapsiblePrimaryItem);
        collapsePolicySubmenu.add(collapsiblePrimaryItem);
        JRadioButtonMenuItem collapsibleSecondaryItem = new JRadioButtonMenuItem("Collapse secondary dimension");
        collapsibleSecondaryItem.addActionListener(l);
        collapsePolicyGroup.add(collapsibleSecondaryItem);
        collapsePolicySubmenu.add(collapsibleSecondaryItem);
        switch (v.getConfig().getCollapsePolicy())
        {
            case 0:
            {
                staticItem.setSelected(true); break;
            }
            case 1:
            {
                collapsiblePrimaryItem.setSelected(true); break;
            }
            case 2:
            {
                collapsibleSecondaryItem.setSelected(true); break;
            }
        }
        add(collapsePolicySubmenu);
        
        addSeparator();
        
        JMenu nodeShapePolicySubmenu = new JMenu("Node shape");
        ButtonGroup nodeShapePolicyGroup = new ButtonGroup();
        JRadioButtonMenuItem pointShapeItem = new JRadioButtonMenuItem("Point nodes");
        pointShapeItem.addActionListener(l);
        nodeShapePolicyGroup.add(pointShapeItem);
        nodeShapePolicySubmenu.add(pointShapeItem);
        JRadioButtonMenuItem boxShapeItem = new JRadioButtonMenuItem("Box nodes");
        boxShapeItem.addActionListener(l);
        nodeShapePolicyGroup.add(boxShapeItem);
        nodeShapePolicySubmenu.add(boxShapeItem);
        JRadioButtonMenuItem ovalShapeItem = new JRadioButtonMenuItem("Oval nodes");
        ovalShapeItem.addActionListener(l);
        nodeShapePolicyGroup.add(ovalShapeItem);
        nodeShapePolicySubmenu.add(ovalShapeItem);
        switch (v.getConfig().getNodeShapePolicy())
        {
            case 0:
            {
                pointShapeItem.setSelected(true); break;
            }
            case 1:
            {
                boxShapeItem.setSelected(true); break;
            }
            case 2:
            {
                ovalShapeItem.setSelected(true);
            }
        }
        add(nodeShapePolicySubmenu);
        
        JMenu edgeShapePolicySubmenu = new JMenu("Edge tags");
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

        switch (v.getConfig().getEdgeTagPolicy())
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
        
        
        JMenu lineShapePolicySubmenu = new JMenu("Line shape");
        ButtonGroup lineShapePolicyGroup = new ButtonGroup();
        JRadioButtonMenuItem directLinesItem = new JRadioButtonMenuItem("Direct");
        directLinesItem.addActionListener(l);
        lineShapePolicyGroup.add(directLinesItem);
        lineShapePolicySubmenu.add(directLinesItem);
        JRadioButtonMenuItem edgyLinesItem = new JRadioButtonMenuItem("Edgy");
        edgyLinesItem.addActionListener(l);
        lineShapePolicyGroup.add(edgyLinesItem);
        lineShapePolicySubmenu.add(edgyLinesItem);
        JRadioButtonMenuItem invisibleLinesItem = new JRadioButtonMenuItem("Invisible");
        invisibleLinesItem.addActionListener(l);
        lineShapePolicyGroup.add(invisibleLinesItem);
        lineShapePolicySubmenu.add(invisibleLinesItem);
        switch (v.getConfig().getLineShapePolicy())
        {
            case 0:
            {
                directLinesItem.setSelected(true); break;
            }
            case 1:
            {
                edgyLinesItem.setSelected(true); break;
            }
            case 2:
            {
                invisibleLinesItem.setSelected(true);
            }
        }
        add(lineShapePolicySubmenu);      
        
        JMenu nodePositionPolicySubmenu = new JMenu("Node positioning");
        ButtonGroup nodePositionPolicyGroup = new ButtonGroup();
        JRadioButtonMenuItem centeredNodesItem = new JRadioButtonMenuItem("Centered");
        centeredNodesItem.addActionListener(l);
        nodePositionPolicyGroup.add(centeredNodesItem);
        nodePositionPolicySubmenu.add(centeredNodesItem);
        JRadioButtonMenuItem leftAlignedNodesItem = new JRadioButtonMenuItem("Left alignment");
        leftAlignedNodesItem.addActionListener(l);
        nodePositionPolicyGroup.add(leftAlignedNodesItem);
        nodePositionPolicySubmenu.add(leftAlignedNodesItem);
        JRadioButtonMenuItem rightAlignedNodesItem = new JRadioButtonMenuItem("Right alignment");
        rightAlignedNodesItem.addActionListener(l);
        nodePositionPolicyGroup.add(rightAlignedNodesItem);
        nodePositionPolicySubmenu.add(rightAlignedNodesItem);
        switch (v.getConfig().getNodePositionPolicy())
        {
            case 0:
            {
                centeredNodesItem.setSelected(true); break;
            }
            case 1:
            {
                leftAlignedNodesItem.setSelected(true); break;
            }
            case 2:
            {
                rightAlignedNodesItem.setSelected(true);
            }
        }
        add(nodePositionPolicySubmenu);
        
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
              
        JMenu secondaryLineShapePolicySubmenu = new JMenu("Secondary line shape");
        ButtonGroup secondaryLineShapePolicyGroup = new ButtonGroup();
        JRadioButtonMenuItem directSecondaryLinesItem = new JRadioButtonMenuItem("Direct");
        directSecondaryLinesItem.setActionCommand("Secondary direct");
        directSecondaryLinesItem.addActionListener(l);
        secondaryLineShapePolicyGroup.add(directSecondaryLinesItem);
        secondaryLineShapePolicySubmenu.add(directSecondaryLinesItem);
        directSecondaryLinesItem.setEnabled(false);
        JRadioButtonMenuItem edgySecondaryLinesItem = new JRadioButtonMenuItem("Edgy");
        directSecondaryLinesItem.setActionCommand("Secondary edgy");
        edgySecondaryLinesItem.addActionListener(l);
        secondaryLineShapePolicyGroup.add(edgySecondaryLinesItem);
        secondaryLineShapePolicySubmenu.add(edgySecondaryLinesItem);
        JRadioButtonMenuItem invisibleSecondaryLinesItem = new JRadioButtonMenuItem("Invisible");
        invisibleSecondaryLinesItem.setActionCommand("Secondary invisible");
        invisibleSecondaryLinesItem.addActionListener(l);
        secondaryLineShapePolicyGroup.add(invisibleSecondaryLinesItem);
        secondaryLineShapePolicySubmenu.add(invisibleSecondaryLinesItem);
        switch (v.getConfig().getSecondaryLineShapePolicy())
        {
            case 0:
            {
                directSecondaryLinesItem.setSelected(true); break;
            }
            case 1:
            {
                edgySecondaryLinesItem.setSelected(true); break;
            }
            case 2:
            {
                invisibleSecondaryLinesItem.setSelected(true);
            }
        }
        add(secondaryLineShapePolicySubmenu);
        
        JCheckBoxMenuItem displaySecondDimensionItem = new JCheckBoxMenuItem("Display second dimension");
        displaySecondDimensionItem.addActionListener(l);
        if (v.isSecondDimensionDisplayed())
        {
            displaySecondDimensionItem.setSelected(true);
        }
        add(displaySecondDimensionItem);
        
        JMenuItem swapDimensionsItem = new JMenuItem("Swap dimensions");
        swapDimensionsItem.addActionListener(l);
        add(swapDimensionsItem);  
       
        
        addSeparator();
        
        JMenuItem exportPNGItem = new JMenuItem("Save as PNG");
        exportPNGItem.addActionListener(l);
        add(exportPNGItem);
        
        JMenuItem indentedTextItem = new JMenuItem("Save as Indented Text File");
        indentedTextItem.addActionListener(l);
        add(indentedTextItem);
    }
    
    public static JPopupMenu getMenu(ActionListener l, KahinaTreeView v)
    {
        return new KahinaTreeViewContextMenu(l, v);
    }
}
