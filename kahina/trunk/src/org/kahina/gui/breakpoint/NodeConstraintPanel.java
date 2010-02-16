package org.kahina.gui.breakpoint;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.text.JTextComponent;

import org.kahina.breakpoint.PatternFormatException;
import org.kahina.breakpoint.TreeNodePattern;
import org.kahina.breakpoint.TreePatternNode;
import org.kahina.control.KahinaController;
import org.kahina.control.KahinaListener;
import org.kahina.control.event.KahinaEvent;

public class NodeConstraintPanel extends JPanel implements ActionListener, KahinaListener
{
    KahinaController control;
    
    NodeConstraintOptions constrOptions;  
    
    SingleNodeConstraintPanel constPanel;
    BooleanOperationsPanel boolOpsPanel;
    BreakpointEditorHintPanel hintPanel;
    
    //store internally which kind of connective is being built; 
    //changes coordinated with boolean connector panels via event system 
    private int selectionMode;
    
    public NodeConstraintPanel(KahinaController control)
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
        add(bottomPanel);
        
        hintPanel = new BreakpointEditorHintPanel();   
        add(hintPanel);
        
        constPanel = new SingleNodeConstraintPanel(constrOptions, control);
        constPanel.setHintPanel(hintPanel);
        constPanel.setSynchronized(true);
        add(constPanel);  
        
        selectionMode = -1;
    }
    
    public NodeConstraintPanel(KahinaController control, NodeConstraintOptions constrOptions)
    {
        this(control);
        this.constrOptions = constrOptions;
        constPanel.setConstrOptions(constrOptions);
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("negOperation"))
        {
            if (constPanel.getMarkedPattern() != null)
            {
                constPanel.introduceNegation(constPanel.getMarkedPattern());
            }
            else
            {
                hint("Select first the constraint to be negated.", Color.RED);
            }
        }
        else if (s.equals("andOperation"))
        {
            if (constPanel.getMarkedPattern() != null)
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
            if (constPanel.getMarkedPattern() != null)
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
            if (constPanel.getMarkedPattern() != null)
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
        hintPanel.setEnabled(true);
        boolOpsPanel.setEnabled(true);
        validate();
    }
    
    public void deactivateAllComponents()
    {
        clear();
        hintPanel.setEnabled(false);
        boolOpsPanel.setEnabled(false);
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
        else if (event.getEditorEventType() == BreakpointEditorEvent.SYNCHRONIZE_EDITOR_VIEWS)
        {
           if (event.getPanel() != constPanel && constPanel != null)
           {
               constPanel.takeOverStructure(event.getPanel());
           }
        }
    }
    
    public void clear()
    {
        if (constPanel != null)
        {
            remove(constPanel);  
            constPanel = null;
        }
    }
    
    public void displayNodeConstraint(TreePatternNode n)
    {
        constPanel = new SingleNodeConstraintPanel(constrOptions, control, n);
        constPanel.setHintPanel(hintPanel);
        constPanel.setSynchronized(true);
        this.add(constPanel);
    }
}
