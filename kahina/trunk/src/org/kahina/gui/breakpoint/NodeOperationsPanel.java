package org.kahina.gui.breakpoint;

import java.awt.Dimension;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;

public class NodeOperationsPanel extends JPanel
{
    JButton addChildOperationButton;
    JButton setParentOperationButton;
    JButton removeOperationButton;
    
    public NodeOperationsPanel(ActionListener listener)
    {
        setLayout(new BoxLayout(this, BoxLayout.LINE_AXIS));
        setBorder(BorderFactory.createTitledBorder("Node Operations"));
        
        addChildOperationButton = new JButton("Add Child");
        addChildOperationButton.setActionCommand("addChild");
        addChildOperationButton.addActionListener(listener);
        add(addChildOperationButton);
        add(Box.createRigidArea(new Dimension(10,0)));
        
        setParentOperationButton = new JButton("Set Parent");
        setParentOperationButton.setActionCommand("setParent");
        setParentOperationButton.addActionListener(listener);
        add(setParentOperationButton);
        add(Box.createRigidArea(new Dimension(10,0)));
        
        removeOperationButton = new JButton("Remove Node");
        removeOperationButton.setActionCommand("removeNode");
        removeOperationButton.addActionListener(listener);
        add(removeOperationButton);
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
        addChildOperationButton.setEnabled(true);
        //setParentOperationButton.setEnabled(true);
        removeOperationButton.setEnabled(true);
    }
    
    public void deactivateAllComponents()
    {
        addChildOperationButton.setEnabled(false);
        setParentOperationButton.setEnabled(false);
        removeOperationButton.setEnabled(false);
    }
}
