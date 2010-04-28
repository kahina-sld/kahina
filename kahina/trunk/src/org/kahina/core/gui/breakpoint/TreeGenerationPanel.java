package org.kahina.core.gui.breakpoint;

import java.awt.Dimension;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;

public class TreeGenerationPanel extends JPanel
{
	private static final long serialVersionUID = -2045828338668568984L;
	
	JButton addRandomNodeButton;
    JButton randomTreeGrowthButton;
    JButton stopTreeGrowthButton;
    JButton discardTreeButton;
    
    public TreeGenerationPanel(ActionListener listener)
    {
        setLayout(new BoxLayout(this, BoxLayout.LINE_AXIS));
        setBorder(BorderFactory.createTitledBorder("Tree Generation"));
        
        addRandomNodeButton = new JButton("Random Node");
        addRandomNodeButton.setActionCommand("addRandomNode");
        addRandomNodeButton.addActionListener(listener);
        add(addRandomNodeButton);
        add(Box.createRigidArea(new Dimension(10,0)));
        
        randomTreeGrowthButton = new JButton("Random Growth");
        randomTreeGrowthButton.setActionCommand("randomGrowth");
        randomTreeGrowthButton.addActionListener(listener);
        add(randomTreeGrowthButton);
        add(Box.createRigidArea(new Dimension(10,0)));
        
        stopTreeGrowthButton = new JButton("Stop Growth");
        stopTreeGrowthButton.setActionCommand("stopGrowth");
        stopTreeGrowthButton.addActionListener(listener);
        add(stopTreeGrowthButton);
        add(Box.createRigidArea(new Dimension(10,0)));
        
        discardTreeButton = new JButton("Discard");
        discardTreeButton.setActionCommand("discardTree");
        discardTreeButton.addActionListener(listener);
        add(discardTreeButton);
        add(Box.createRigidArea(new Dimension(10,0)));
    }
    
    public void setEnabled(boolean enabled)
    {
        if (enabled)
        {
            activateAllComponents();
        }
        else
        {
            deactivateAllComponents();
        }
    }
    
    public void activateAllComponents()
    {
        addRandomNodeButton.setEnabled(true);
        randomTreeGrowthButton.setEnabled(true);
        stopTreeGrowthButton.setEnabled(true);
        discardTreeButton.setEnabled(true);
    }
    
    public void deactivateAllComponents()
    {
        addRandomNodeButton.setEnabled(false);
        randomTreeGrowthButton.setEnabled(false);
        stopTreeGrowthButton.setEnabled(false);
        discardTreeButton.setEnabled(false);
    }
}
