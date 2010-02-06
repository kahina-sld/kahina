package org.kahina.gui.breakpoint;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;

import org.kahina.breakpoint.KahinaBreakpoint;

public class BreakpointEditPanel extends JPanel implements ActionListener
{
    private JTabbedPane editTabs;  
    private NodeConstraintPanel nodeConstraintPanel;
    private JPanel treeFragmentPanel; 
    private JLabel nameLabel;
    private JTextField nameField;
    private JButton suggestNameButton;
    private JLabel colorLabel;
    private JButton signalColor;
    private JButton compileBreakpointButton;
    private JButton cancelBreakpointButton;
    
    //the breakpoint this edit panel is operating on
    private KahinaBreakpoint breakpoint;
    
    public BreakpointEditPanel()
    {
        setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
        setBorder(BorderFactory.createTitledBorder("Edit Breakpoint"));    
       
        editTabs = new JTabbedPane();
        editTabs.add(buildNodeConstraintPanel(), "Step Constraint");
        editTabs.add(buildTreeFragmentPanel(), "Decision Tree Pattern");
        
        add(editTabs);
        add(Box.createRigidArea(new Dimension(0,5)));
        
        JPanel optionsPanel = new JPanel();
        optionsPanel.setLayout(new BoxLayout(optionsPanel, BoxLayout.LINE_AXIS));
        optionsPanel.setMaximumSize(new Dimension(800, 30));
        
        nameLabel = new JLabel("Name: ");
        optionsPanel.add(nameLabel);
        optionsPanel.add(Box.createRigidArea(new Dimension(5,0)));
        
        nameField = new JTextField(30);
        optionsPanel.add(nameField);
        optionsPanel.add(Box.createRigidArea(new Dimension(5,0)));
        
        suggestNameButton = new JButton("Suggest");
        suggestNameButton.setActionCommand("suggestName");
        suggestNameButton.addActionListener(this);
        optionsPanel.add(suggestNameButton);
        optionsPanel.add(Box.createRigidArea(new Dimension(10,0)));
        
        colorLabel = new JLabel("Signal color: ");
        optionsPanel.add(colorLabel);
        optionsPanel.add(Box.createRigidArea(new Dimension(5,0)));
        
        signalColor = new JButton("Change");
        signalColor.setBackground(Color.RED);
        signalColor.setActionCommand("changeColor");
        signalColor.addActionListener(this);
        optionsPanel.add(signalColor);
        optionsPanel.add(Box.createRigidArea(new Dimension(5,0)));
        
        compileBreakpointButton = new JButton("Compile");
        compileBreakpointButton.setActionCommand("compileBreakpoint");
        compileBreakpointButton.addActionListener(this);
        optionsPanel.add(compileBreakpointButton);
        optionsPanel.add(Box.createRigidArea(new Dimension(10,0)));
        
        cancelBreakpointButton = new JButton("Cancel");
        cancelBreakpointButton.setActionCommand("cancelBreakpointEditing");
        cancelBreakpointButton.addActionListener(this);
        optionsPanel.add(cancelBreakpointButton);
        
        add(optionsPanel);
        
        setBreakpoint(null);
    }
    
    public void setBreakpoint(KahinaBreakpoint breakpoint)
    {
        if (this.breakpoint == null && breakpoint != null)
        {
            setEnabled(true);
            this.breakpoint = breakpoint;
        }
        if (breakpoint == null)
        {
            breakpoint = null;           
            setEnabled(false);
        }
        else
        {
            showBreakpoint();
        }
    }
    
    private NodeConstraintPanel buildNodeConstraintPanel()
    {
        nodeConstraintPanel = new NodeConstraintPanel();     
        return nodeConstraintPanel;
    }
    
    private JPanel buildTreeFragmentPanel()
    {
        treeFragmentPanel = new JPanel();
        return treeFragmentPanel;
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("changeColor"))
        {
            Color newColor = JColorChooser.showDialog(this,"Choose Background Color",signalColor.getBackground());
            signalColor.setBackground(newColor);
        }
    }
    
    public void setEnabled(boolean enabled)
    {
        super.setEnabled(enabled);
        if (enabled)
        {
            activateAllComponents();
        }
        else
        {
            deactivateAllComponents();
        }
    }
    
    private void activateAllComponents()
    {
        editTabs.setEnabled(true);  
        nodeConstraintPanel.setEnabled(true);
        treeFragmentPanel.setEnabled(true); 
        nameLabel.setEnabled(true);
        nameField.setEnabled(true);
        suggestNameButton.setEnabled(true);
        colorLabel.setEnabled(true);
        signalColor.setEnabled(true);
        compileBreakpointButton.setEnabled(true);
        cancelBreakpointButton.setEnabled(true);
    }
    
    private void deactivateAllComponents()
    {
        editTabs.setEnabled(false);  
        nodeConstraintPanel.setEnabled(false);
        treeFragmentPanel.setEnabled(false); 
        nameLabel.setEnabled(false);
        nameField.setEnabled(false);
        suggestNameButton.setEnabled(false);
        colorLabel.setEnabled(false);
        signalColor.setEnabled(false);
        compileBreakpointButton.setEnabled(false);
        cancelBreakpointButton.setEnabled(false);
    }
    
    public void showBreakpoint()
    {
        
    }
}
