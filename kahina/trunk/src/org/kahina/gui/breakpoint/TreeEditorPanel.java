package org.kahina.gui.breakpoint;

import java.awt.Dimension;
import java.awt.Insets;
import java.util.HashMap;

import javax.swing.JButton;
import javax.swing.JPanel;

public class TreeEditorPanel extends JPanel
{
    TreeFragmentPanel fragment;
    
    private HashMap<SingleNodeConstraintPanel, Integer> x;
    private HashMap<SingleNodeConstraintPanel, Integer> y;
    
    public TreeEditorPanel(TreeFragmentPanel fragment)
    {
        this.setLayout(null);
        this.fragment = fragment;
        this.x = new HashMap<SingleNodeConstraintPanel, Integer>();
        this.y = new HashMap<SingleNodeConstraintPanel, Integer>();
    }
    
    public void add(SingleNodeConstraintPanel node)
    {
        System.err.println("Adding node constraint!");  
        super.add(node);
        Insets insets = getInsets();
        Dimension size = node.getPreferredSize();
        node.setBounds(100 + insets.left, 100 + insets.top,
                size.width, size.height);
    }
    
    public void recalculateCoordinates()
    {
        adaptNodeSizeAndPosition(fragment.rootConstPanel);
    }
    
    public void adaptNodeSizeAndPosition(SingleNodeConstraintPanel node)
    {
        Insets insets = getInsets();
        Dimension size = node.getPreferredSize();
        fragment.rootConstPanel.setBounds(x.get(node) + insets.left , y.get(node) + insets.top, size.width, size.height);
    }
}
