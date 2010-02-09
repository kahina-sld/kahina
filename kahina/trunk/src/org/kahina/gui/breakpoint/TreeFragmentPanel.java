package org.kahina.gui.breakpoint;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.kahina.control.KahinaController;
import org.kahina.control.KahinaListener;
import org.kahina.control.event.KahinaEvent;

public class TreeFragmentPanel extends JPanel implements ActionListener, KahinaListener
{
    KahinaController control;
    
    NodeConstraintOptions constrOptions;  
    
    //store the tree structure of node constraints
    SingleNodeConstraintPanel rootConstPanel;
    List<SingleNodeConstraintPanel> nodeConstPanels;
    
    TreeEditorPanel treePanel;
    BooleanOperationsPanel boolOpsPanel;
    NodeOperationsPanel nodeOpsPanel;
    BreakpointEditorHintPanel hintPanel;
    
    //store internally which kind of connective is being built; 
    //changes coordinated with boolean connector panels via event system 
    private int selectionMode;
    
    public TreeFragmentPanel(KahinaController control)
    {
        this.control = control;
        control.registerListener("breakpoint_editor", this);
        
        constrOptions = new NodeConstraintOptions();
        constrOptions.setStandardOptions();
        
        setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));   

        JPanel bottomPanel = new JPanel();
        bottomPanel.setLayout(new BoxLayout(bottomPanel, BoxLayout.LINE_AXIS));
        
        boolOpsPanel = new BooleanOperationsPanel(this);           
        bottomPanel.add(boolOpsPanel);
        nodeOpsPanel = new NodeOperationsPanel(this);           
        bottomPanel.add(nodeOpsPanel);      
        add(bottomPanel);
        
        hintPanel = new BreakpointEditorHintPanel();   
        add(hintPanel);
        

        treePanel = new TreeEditorPanel(this);
        rootConstPanel = new SingleNodeConstraintPanel(constrOptions, control);
        rootConstPanel.setHintPanel(hintPanel);   
        treePanel.add(rootConstPanel);
        
        JScrollPane treeScroll = new JScrollPane(treePanel);
        add(treeScroll);  
        
        nodeConstPanels = new ArrayList<SingleNodeConstraintPanel>();
        
        selectionMode = -1;
    }
    
    public TreeFragmentPanel(KahinaController control, NodeConstraintOptions constrOptions)
    {
        this(control);
        this.constrOptions = constrOptions;
        rootConstPanel.setConstrOptions(constrOptions);
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("negOperation"))
        {
            if (rootConstPanel.getMarkedPattern() != null)
            {
                rootConstPanel.introduceNegation(rootConstPanel.getMarkedPattern());
            }
            else
            {
                hint("Select first the constraint to be negated.", Color.RED);
            }
        }
        else if (s.equals("andOperation"))
        {
            if (rootConstPanel.getMarkedPattern() != null)
            {
                control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.CHANGE_NODE_SELECTION_MODE, BreakpointEditPanel.PENDING_AND_OPERATION));
                hint("Now select the second conjunct.", Color.BLACK);
            }
            else
            {
                hint("First conjunct must be selected before clicking this button.", Color.RED);
            }
        }
        else if (s.equals("orOperation"))
        {
            if (rootConstPanel.getMarkedPattern() != null)
            {
                control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.CHANGE_NODE_SELECTION_MODE, BreakpointEditPanel.PENDING_OR_OPERATION));
                hint("Now select the second disjunct.", Color.BLACK);
            }
            else
            {
                hint("First disjunct must be selected before clicking this button.", Color.RED);
            }
        }
        else if (s.equals("implOperation"))
        {
            if (rootConstPanel.getMarkedPattern() != null)
            {
                control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.CHANGE_NODE_SELECTION_MODE, BreakpointEditPanel.PENDING_IMPL_OPERATION));
                hint("Now select the consequent.", Color.BLACK);
            }
            else
            {
                hint("Antecedent must be selected before clicking this button.", Color.RED);
            }
        }
    }  
    
    public void hint(String hint)
    {
        hintPanel.hint(hint);
    }
    
    public void hint(String hint, Color color)
    {
        hintPanel.hint(hint,color);
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
        if (rootConstPanel == null)
        {
            rootConstPanel = new SingleNodeConstraintPanel(constrOptions, control);   
            rootConstPanel.setHintPanel(hintPanel);
            treePanel.add(rootConstPanel);
        }
        hintPanel.setEnabled(true);
        boolOpsPanel.setEnabled(true);
        nodeOpsPanel.setEnabled(true);
        validate();
    }
    
    public void deactivateAllComponents()
    {
        if (rootConstPanel != null)
        {
            treePanel.remove(rootConstPanel);
            rootConstPanel = null;
        }
        hintPanel.setEnabled(false);
        boolOpsPanel.setEnabled(false);
        nodeOpsPanel.setEnabled(false);
        validate();
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
            selectionMode = event.getGoalID();
        }
    }
}
