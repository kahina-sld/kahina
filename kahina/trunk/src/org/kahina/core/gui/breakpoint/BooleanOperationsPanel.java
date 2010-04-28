package org.kahina.core.gui.breakpoint;

import java.awt.Dimension;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;

public class BooleanOperationsPanel extends JPanel
{
	private static final long serialVersionUID = -8654088517859959423L;
	
	JButton andOperationButton;
    JButton orOperationButton;
    JButton negOperationButton;
    JButton implOperationButton;
    
    public BooleanOperationsPanel(ActionListener listener)
    {
        setLayout(new BoxLayout(this, BoxLayout.LINE_AXIS));
        setBorder(BorderFactory.createTitledBorder("Boolean Operations"));
        
        andOperationButton = new JButton("&");
        andOperationButton.setActionCommand("andOperation");
        andOperationButton.addActionListener(listener);
        add(andOperationButton);
        add(Box.createRigidArea(new Dimension(10,0)));
        
        orOperationButton = new JButton("v");
        orOperationButton.setActionCommand("orOperation");
        orOperationButton.addActionListener(listener);
        add(orOperationButton);
        add(Box.createRigidArea(new Dimension(10,0)));
        
        negOperationButton = new JButton("~");
        negOperationButton.setActionCommand("negOperation");
        negOperationButton.addActionListener(listener);
        add(negOperationButton);
        add(Box.createRigidArea(new Dimension(10,0)));
        
        implOperationButton = new JButton("->");
        implOperationButton.setActionCommand("implOperation");
        implOperationButton.addActionListener(listener);
        add(implOperationButton);
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
        andOperationButton.setEnabled(true);
        orOperationButton.setEnabled(true);
        negOperationButton.setEnabled(true);
        implOperationButton.setEnabled(true);
    }
    
    public void deactivateAllComponents()
    {
        andOperationButton.setEnabled(false);
        orOperationButton.setEnabled(false);
        negOperationButton.setEnabled(false);
        implOperationButton.setEnabled(false);
    }
}
