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
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.kahina.breakpoint.KahinaBreakpoint;
import org.kahina.breakpoint.TreeAutomaton;
import org.kahina.control.KahinaController;
import org.kahina.control.KahinaListener;
import org.kahina.control.event.KahinaEvent;
import org.kahina.control.event.KahinaSystemEvent;

public class BreakpointEditorWindow extends JFrame implements ActionListener, KahinaListener, ListSelectionListener
{
    KahinaController control;
    
    JPanel breakpointListPanel;
    JList breakpointList;
    
    BreakpointEditPanel editPanel;
    
    List<KahinaBreakpoint> breakpoints;
    List<TreeAutomaton> compiledBreakpoints;
    int curID; //list ID of the breakpoint we are editing
    
    public BreakpointEditorWindow(KahinaController control)
    {       
        this.control = control;
        control.registerListener("breakpoint_editor", this);
        this.setTitle("Kahina Breakpoint Editor");
        this.setSize(800,600);
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
       
        breakpoints = new ArrayList<KahinaBreakpoint>();
        compiledBreakpoints = new ArrayList<TreeAutomaton>();
        curID = -1;
        
        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.LINE_AXIS));    
        mainPanel.add(buildLeftPanel()); 
        mainPanel.add(buildRightPanel());
        
        add(mainPanel);
        
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
        breakpointList.addListSelectionListener(this);
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
        editPanel = new BreakpointEditPanel(control);
        return editPanel;       
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
    }
    
    public void processEvent(KahinaEvent e)
    {
        if (e.getType().equals("breakpoint_editor"))
        {
            processBreakpointEvent((BreakpointEditorEvent) e);
        }
    }
    
    private void processBreakpointEvent(BreakpointEditorEvent e)
    {
        switch (e.getEditorEventType())
        {
            case BreakpointEditorEvent.NEW_BREAKPOINT:
            {
                KahinaBreakpoint newBreakpoint = new KahinaBreakpoint();
                breakpoints.add(newBreakpoint);
                compiledBreakpoints.add(new TreeAutomaton(newBreakpoint));
                breakpointList.setListData(breakpoints.toArray());
                break;
            }
            case BreakpointEditorEvent.TEST_BREAKPOINTS:
            {
                BreakpointTestWindow w = new BreakpointTestWindow(compiledBreakpoints, control);
                w.setVisible(true);
                break;
            }
            case BreakpointEditorEvent.COMPILE_CURRENT_BREAKPOINT:
            {
                TreeAutomaton compiledBreakpoint = breakpoints.get(curID).compile();
                System.err.println("Compiled Tree Automaton:\n" + compiledBreakpoint.toString());
                compiledBreakpoints.set(curID, compiledBreakpoint);
            }
        }
    }
    
    public void valueChanged(ListSelectionEvent e)
    {
        curID = breakpointList.getSelectedIndex();
        if (curID == -1)
        {
            editPanel.setBreakpoint(null);
        }
        else
        {
            editPanel.setBreakpoint(breakpoints.get(curID));
        }
    }
}
