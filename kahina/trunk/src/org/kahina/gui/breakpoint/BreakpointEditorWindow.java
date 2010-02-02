package org.kahina.gui.breakpoint;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenuBar;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import org.kahina.breakpoint.KahinaBreakpoint;
import org.kahina.control.KahinaController;
import org.kahina.control.event.KahinaSystemEvent;

public class BreakpointEditorWindow extends JFrame implements ActionListener
{
    KahinaController control;
    
    JPanel breakpointListPanel;
    JList breakpointList;
    
    JPanel editPanel;
    JTabbedPane editTabs;  
    JPanel nodeConstraintPanel;
    JPanel treeFragmentPanel;  
    
    JButton signalColor;
    
    List<KahinaBreakpoint> breakpoints;
    int curID; //ID of the breakpoint we are editing
    
    public BreakpointEditorWindow(KahinaController control)
    {       
        this.control = control;
        this.setTitle("Kahina Breakpoint Editor");
        this.setSize(640,480);
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
       
        breakpoints = new ArrayList<KahinaBreakpoint>();
        curID = -1;
        
        add(buildLeftPanel()); 
        add(buildRightPanel());
       
        
        JMenuBar menuBar = new JMenuBar();
        menuBar.add(new BreakpointEditorFileMenu(control));
        this.setJMenuBar(menuBar);
    }
    
    private JPanel buildLeftPanel()
    {
        breakpointListPanel = new JPanel();
        breakpointListPanel.setLayout(new BoxLayout(breakpointListPanel, BoxLayout.PAGE_AXIS));
        breakpointListPanel.setBorder(BorderFactory.createTitledBorder("Breakpoints"));

        
        JButton newBreakpointButton = new JButton("New");
        newBreakpointButton.setActionCommand("newBreakpoint");
        newBreakpointButton.addActionListener(this);
        newBreakpointButton.setAlignmentX(CENTER_ALIGNMENT);
        breakpointListPanel.add(newBreakpointButton);
        breakpointListPanel.add(Box.createRigidArea(new Dimension(0,5)));
        
        JButton activateBreakpointButton = new JButton("Activate");
        activateBreakpointButton.setActionCommand("activateBreakpoint");
        activateBreakpointButton.addActionListener(this);
        activateBreakpointButton.setAlignmentX(CENTER_ALIGNMENT);
        breakpointListPanel.add(activateBreakpointButton, BorderLayout.NORTH);
        breakpointListPanel.add(Box.createRigidArea(new Dimension(0,5)));
        
        breakpointList = new JList();
        breakpointList.setListData(breakpoints.toArray());
        JScrollPane listScroller = new JScrollPane(breakpointList);
        listScroller.setPreferredSize(new Dimension(250, 80));
        listScroller.setMaximumSize(new Dimension(300, 1000));
        listScroller.setAlignmentX(CENTER_ALIGNMENT);
        breakpointListPanel.add(listScroller);
        
        breakpointListPanel.add(Box.createRigidArea(new Dimension(0,5)));
        
        JButton deactivateBreakpointButton = new JButton("Deactivate");
        deactivateBreakpointButton.setActionCommand("deactivateBreakpoint");
        deactivateBreakpointButton.addActionListener(this);
        deactivateBreakpointButton.setAlignmentX(CENTER_ALIGNMENT);
        breakpointListPanel.add(deactivateBreakpointButton);
        breakpointListPanel.add(Box.createRigidArea(new Dimension(0,5)));
        
        JButton removeBreakpointButton = new JButton("Remove");
        removeBreakpointButton.setActionCommand("removeBreakpoint");
        removeBreakpointButton.addActionListener(this);
        removeBreakpointButton.setAlignmentX(CENTER_ALIGNMENT);
        breakpointListPanel.add(removeBreakpointButton, BorderLayout.SOUTH);
        
        return breakpointListPanel;
    }
    
    private JPanel buildRightPanel()
    {
        editPanel = new JPanel();
        editPanel.setLayout(new BoxLayout(editPanel, BoxLayout.PAGE_AXIS));
        editPanel.setBorder(BorderFactory.createTitledBorder("Edit Breakpoint"));    
       
        editTabs = new JTabbedPane();
        editTabs.add(buildNodeConstraintPanel(), "Node Constraint");
        editTabs.add(buildTreeFragmentPanel(), "Tree Fragment");
        
        editPanel.add(editTabs);
        editPanel.add(Box.createRigidArea(new Dimension(0,5)));
        
        JPanel optionsPanel = new JPanel();
        optionsPanel.setLayout(new BoxLayout(optionsPanel, BoxLayout.LINE_AXIS));
        
        JLabel nameLabel = new JLabel("Name: ");
        optionsPanel.add(nameLabel);
        optionsPanel.add(Box.createRigidArea(new Dimension(5,0)));
        
        JTextField nameField = new JTextField(30);
        optionsPanel.add(nameField);
        optionsPanel.add(Box.createRigidArea(new Dimension(5,0)));
        
        JButton suggestNameButton = new JButton("Suggest");
        suggestNameButton.setActionCommand("suggestName");
        suggestNameButton.addActionListener(this);
        optionsPanel.add(suggestNameButton);
        optionsPanel.add(Box.createRigidArea(new Dimension(10,0)));
        
        JLabel colorLabel = new JLabel("Signal color: ");
        optionsPanel.add(colorLabel);
        optionsPanel.add(Box.createRigidArea(new Dimension(5,0)));
        
        signalColor = new JButton("Change");
        signalColor.setBackground(Color.RED);
        signalColor.setActionCommand("changeColor");
        signalColor.addActionListener(this);
        optionsPanel.add(signalColor);
        optionsPanel.add(Box.createRigidArea(new Dimension(5,0)));
        
        JButton compileBreakpointButton = new JButton("Compile");
        compileBreakpointButton.setActionCommand("compileBreakpoint");
        compileBreakpointButton.addActionListener(this);
        optionsPanel.add(compileBreakpointButton);
        optionsPanel.add(Box.createRigidArea(new Dimension(10,0)));
        
        JButton cancelBreakpointButton = new JButton("Cancel");
        cancelBreakpointButton.setActionCommand("cancelBreakpointEditing");
        cancelBreakpointButton.addActionListener(this);
        optionsPanel.add(cancelBreakpointButton);
        
        editPanel.add(optionsPanel);
        
        return editPanel;
    }
    
    private JPanel buildNodeConstraintPanel()
    {
        JPanel nodeConstraintPanel = new JPanel();
        nodeConstraintPanel.setLayout(new BoxLayout(nodeConstraintPanel, BoxLayout.PAGE_AXIS));
        
        JPanel elConstPanel = new JPanel();
        elConstPanel.setLayout(new GridBagLayout());
        GridBagConstraints c = new GridBagConstraints();
        elConstPanel.setBorder(BorderFactory.createTitledBorder("Elementary Constraints"));
        
        String[] constTypeStrings = { "node label", "edge label", "node id", "type"};

        JComboBox constTypeChoice = new JComboBox(constTypeStrings);
        constTypeChoice.addActionListener(this);
        
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridx = 0;
        c.gridy = 0;
        elConstPanel.add(constTypeChoice, c);
        
        String[] compTypeStrings = { "equals", "contains", "starts with", "ends with", "matches", "=", "<=", ">=", "<", ">", "!="};

        JComboBox compTypeChoice = new JComboBox(compTypeStrings);
        compTypeChoice.addActionListener(this);
        
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridx = 1;
        c.gridy = 0;
        elConstPanel.add(compTypeChoice, c);
        
        JTextField valueInput = new JTextField(50);

        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridx = 2;
        c.gridy = 0;
        elConstPanel.add(valueInput, c);   
    
        nodeConstraintPanel.add(elConstPanel);
        
        JPanel boolOpsPanel = new JPanel();
        boolOpsPanel.setLayout(new BoxLayout(boolOpsPanel, BoxLayout.LINE_AXIS));
        boolOpsPanel.setBorder(BorderFactory.createTitledBorder("Boolean Operations"));
        
        JButton andOperationButton = new JButton("And");
        andOperationButton.setActionCommand("andOperation");
        andOperationButton.addActionListener(this);
        boolOpsPanel.add(andOperationButton);
        boolOpsPanel.add(Box.createRigidArea(new Dimension(10,0)));
        
        JButton orOperationButton = new JButton("Or");
        orOperationButton.setActionCommand("orOperation");
        orOperationButton.addActionListener(this);
        boolOpsPanel.add(orOperationButton);
        boolOpsPanel.add(Box.createRigidArea(new Dimension(10,0)));
        
        JButton negOperationButton = new JButton("Not");
        negOperationButton.setActionCommand("negOperation");
        negOperationButton.addActionListener(this);
        boolOpsPanel.add(negOperationButton);
        boolOpsPanel.add(Box.createRigidArea(new Dimension(10,0)));
        
        JButton implOperationButton = new JButton("Impl");
        implOperationButton.setActionCommand("implOperation");
        implOperationButton.addActionListener(this);
        boolOpsPanel.add(implOperationButton);
        boolOpsPanel.add(Box.createRigidArea(new Dimension(10,0)));
        
        nodeConstraintPanel.add(boolOpsPanel);
        
        return nodeConstraintPanel;
    }
    
    private JPanel buildTreeFragmentPanel()
    {
        return new JPanel();
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("newBreakpoint"))
        {
            control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.NEW_BREAKPOINT));
        }
        else if (s.equals("activateBreakpoint"))
        {
            control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.ACTIVATE_BREAKPOINT, curID));
        }
        else if (s.equals("deactivateBreakpoint"))
        {
            control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.DEACTIVATE_BREAKPOINT, curID));
        }
        else if (s.equals("removeBreakpoint"))
        {
            control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.REMOVE_BREAKPOINT, curID));
        }
        else if (s.equals("changeColor"))
        {
            Color newColor = JColorChooser.showDialog(this,"Choose Background Color",signalColor.getBackground());
            signalColor.setBackground(newColor);
        }
    }
}
