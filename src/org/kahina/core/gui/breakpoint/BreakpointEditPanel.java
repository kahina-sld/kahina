package org.kahina.core.gui.breakpoint;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;

import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.breakpoint.KahinaBreakpoint;

public class BreakpointEditPanel extends JPanel implements ActionListener, KahinaListener
{

	private static final long serialVersionUID = 1696100673150672096L;

	private KahinaController control;
    
    private JTabbedPane editTabs;  
    private NodeConstraintPanel nodeConstraintPanel;
    private TreeFragmentPanel treeFragmentPanel; 
    private JLabel nameLabel;
    private JTextField nameField;
    private JButton suggestNameButton;
    private JLabel colorLabel;
    private JButton signalColor;
    private JButton showAutomatonButton;
    
    //the breakpoint this edit panel is operating on
    protected KahinaBreakpoint breakpoint;
    
    //selection mode constants; used by BooleanConnectorPanels and NodeConstraintPanels to coordinate their states
    public static final int NO_PENDING_OPERATION = -1;
    public static final int PENDING_AND_OPERATION = 0;
    public static final int PENDING_OR_OPERATION = 1;
    public static final int PENDING_IMPL_OPERATION = 2;
    
    public BreakpointEditPanel(KahinaController control)
    {
        this.control = control;
        control.registerListener("breakpoint_editor", this);
        
        setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
        setBorder(BorderFactory.createTitledBorder("Edit Breakpoint"));    
       
        addAllComponents();
        
        setBreakpoint(null);
    }
    
    protected void addAllComponents()
    {
    	editTabs = new JTabbedPane();
        editTabs.add("Step Constraint", buildNodeConstraintPanel(control));
        editTabs.add("Decision Tree Pattern", buildTreeFragmentPanel(control));
        
        add(editTabs);
        add(Box.createRigidArea(new Dimension(0,5)));
        
        JPanel optionsPanel = new JPanel();
        optionsPanel.setLayout(new BoxLayout(optionsPanel, BoxLayout.LINE_AXIS));
        optionsPanel.setMaximumSize(new Dimension(800, 30));
        
        nameLabel = new JLabel("Name: ");
        optionsPanel.add(nameLabel);
        optionsPanel.add(Box.createRigidArea(new Dimension(5,0)));
        
        nameField = new JTextField(30);
        nameField.addKeyListener(new NameFieldKeyListener());
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
        
        showAutomatonButton = new JButton("Show Automaton");
        showAutomatonButton.setActionCommand("showAutomaton");
        showAutomatonButton.addActionListener(this);
        optionsPanel.add(showAutomatonButton);
        optionsPanel.add(Box.createRigidArea(new Dimension(10,0)));
        
        add(optionsPanel);
    }
    
    public void setBreakpoint(KahinaBreakpoint breakpoint)
    {
        if (this.breakpoint == null && breakpoint != null)
        {
            setEnabled(true);
            this.breakpoint = breakpoint;
        }
        else
        {
            clearComponents();
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
    
    private NodeConstraintPanel buildNodeConstraintPanel(KahinaController control)
    {
        nodeConstraintPanel = new NodeConstraintPanel(control);     
        return nodeConstraintPanel;
    }
    
    private TreeFragmentPanel buildTreeFragmentPanel(KahinaController control)
    {
        treeFragmentPanel = new TreeFragmentPanel(control);
        return treeFragmentPanel;
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("changeColor"))
        {
            Color newColor = JColorChooser.showDialog(this,"Choose Background Color",signalColor.getBackground());
            signalColor.setBackground(newColor);
            breakpoint.setSignalColor(newColor);
        }
        else if (s.equals("showAutomaton"))
        {
            updateBreakpointPattern();
            JOptionPane.showMessageDialog(this,breakpoint.compile(),"Compiled Automaton",JOptionPane.INFORMATION_MESSAGE);
        }
        else if (s.equals("suggestName"))
        {
            breakpoint.setName(breakpoint.getPattern().toString());
            nameField.setText(breakpoint.getName());
            control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.BREAKPOINT_NAME_UPDATE));
        }
    }
    
    public void updateBreakpointPattern()
    {
        breakpoint.setPattern(treeFragmentPanel.getTreePattern());
    }
    
    @Override
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
    
    protected void activateAllComponents()
    {
        editTabs.setEnabled(true);  
        nodeConstraintPanel.setEnabled(true);
        treeFragmentPanel.setEnabled(true); 
        nameLabel.setEnabled(true);
        nameField.setEnabled(true);
        suggestNameButton.setEnabled(true);
        colorLabel.setEnabled(true);
        signalColor.setEnabled(true);
        showAutomatonButton.setEnabled(true);
    }
    
    protected void deactivateAllComponents()
    {
        editTabs.setEnabled(false);  
        nodeConstraintPanel.setEnabled(false);
        treeFragmentPanel.setEnabled(false); 
        nameLabel.setEnabled(false);
        nameField.setEnabled(false);
        suggestNameButton.setEnabled(false);
        colorLabel.setEnabled(false);
        signalColor.setEnabled(false);
        signalColor.setBackground(Color.GRAY);
        showAutomatonButton.setEnabled(false);
    }
    
    public void processEvent(KahinaEvent event)
    {
        if (event.getType().equals("breakpoint_editor"))
        {
            processEvent((BreakpointEditorEvent) event);
        }
    }
    
    public void processEvent(BreakpointEditorEvent event)
    {
        if (event.getEditorEventType() == BreakpointEditorEvent.CHANGE_NODE_SELECTION_MODE)
        {
            breakpoint.setPattern(treeFragmentPanel.getTreePattern());
        }
    }
    
    public void showBreakpoint()
    {
        signalColor.setBackground(breakpoint.getSignalColor());
        nameField.setText(breakpoint.getName());
        nodeConstraintPanel.displayNodeConstraint(breakpoint.getPattern().getRoot());
        treeFragmentPanel.clear();
        treeFragmentPanel.displayTreePattern(breakpoint.getPattern());
        validate();
    }
    
    public void clearComponents()
    {
        nodeConstraintPanel.clear();
        treeFragmentPanel.clear();
    }
    
    private class NameFieldKeyListener implements KeyListener
    {
        
        public NameFieldKeyListener()
        {
        }
        
        public void keyPressed(KeyEvent e) 
        {
        }

        public void keyReleased(KeyEvent e) 
        {
            String val = nameField.getText();
            breakpoint.setName(val);
            control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.BREAKPOINT_NAME_UPDATE));
        }

        public void keyTyped(KeyEvent e) 
        {

        }
    }
}
