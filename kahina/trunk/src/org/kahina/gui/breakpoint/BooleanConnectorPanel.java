package org.kahina.gui.breakpoint;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.JPanel;

import org.kahina.breakpoint.TreeNodePattern;

public class BooleanConnectorPanel extends JPanel
{
    NodeConstraintPanel nodeConstPanel;
    
    Map<TreeNodePattern, Integer> xCoord;
    Map<TreeNodePattern, Integer> yCoord; 
    
    int xDim;
    int yDim;
    
    TreeNodePattern markedPattern;
    
    public BooleanConnectorPanel(NodeConstraintPanel nodeConstPanel)
    {   
        this.nodeConstPanel = nodeConstPanel;
        xCoord = new HashMap<TreeNodePattern, Integer>();
        yCoord = new HashMap<TreeNodePattern, Integer>();  
        
        markedPattern = null;
    }
    
    public void adaptSize()
    {
        xDim = 30;
        yDim = nodeConstPanel.basePattern.size() * 24;
        
        setMinimumSize(new Dimension(xDim,yDim));
        setPreferredSize(new Dimension(xDim,yDim));
        setSize(new Dimension(xDim,yDim));
    }
    
    public void recalculateCoordinates()
    {
        //first define the base nodes
        List<TreeNodePattern> basePattern = nodeConstPanel.basePattern;
        for (int i = 0; i < basePattern.size(); i++)
        {
            TreeNodePattern pat = basePattern.get(i);
            xCoord.put(pat, 3);
            yCoord.put(pat, i * 24 + 12);     
        }
        //TODO: then calculate the other positions based on these
    }
    
    private int getXfor(TreeNodePattern pat)
    {
        return xCoord.get(pat);
    }
    
    private int getYfor(TreeNodePattern pat)
    {
        return yCoord.get(pat);
    }
    
    public TreeNodePattern getMarkedPattern()
    {
        return markedPattern;
    }
    
    public void setMarkedPattern(TreeNodePattern markedPattern)
    {
        this.markedPattern = markedPattern;
    }
    
    public void paintComponent(Graphics cnv)
    {
        cnv.setColor(Color.WHITE);
        cnv.fillRect(0, 0, xDim, yDim);
        cnv.setColor(Color.BLACK);
        cnv.drawRect(0, 0, xDim - 1, yDim - 1);
        for (TreeNodePattern pat : xCoord.keySet())
        {
            cnv.setColor(Color.BLACK);
            if (markedPattern == pat)
            {
                cnv.setColor(Color.RED);
            }
            int x = xDim - getXfor(pat);
            int y = getYfor(pat);
            switch (pat.getType())
            {       
                case (TreeNodePattern.NEGATION):
                {
                    cnv.drawString("NOT", x, y);
                    break;
                }
                case (TreeNodePattern.CONJUNCTION):
                {
                    cnv.drawString("AND", x, y);
                    break;
                }
                case (TreeNodePattern.DISJUNCTION):
                {
                    cnv.drawString("OR", x, y);
                    break;
                }
                case (TreeNodePattern.IMPLICATION):
                {
                    cnv.drawString("IMPL", x, y);
                    break;
                }
            }
            cnv.drawRect(x-8, y-8, 16, 16);
        }
    }
}
