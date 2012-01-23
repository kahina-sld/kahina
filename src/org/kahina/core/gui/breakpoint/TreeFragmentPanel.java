package org.kahina.core.gui.breakpoint;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.control.patterns.TreePattern;
import org.kahina.core.control.patterns.TreePatternNode;
import org.kahina.core.event.KahinaEvent;

public class TreeFragmentPanel extends JPanel implements ActionListener, KahinaListener
{
    KahinaController control;
    
    NodeConstraintOptions constrOptions;  
    
    //store the tree structure of node constraints
    private SingleNodeConstraintPanel rootConstPanel;
    //List<SingleNodeConstraintPanel> nodeConstPanels;
    private HashMap<SingleNodeConstraintPanel, List<SingleNodeConstraintPanel>> children;
    private HashMap<SingleNodeConstraintPanel, SingleNodeConstraintPanel> parents;  
    
    TreeEditorPanel treePanel;
    BooleanOperationsPanel boolOpsPanel;
    NodeOperationsPanel nodeOpsPanel;
    BreakpointEditorHintPanel hintPanel;
    
    //store internally which kind of connective is being built; 
    //changes coordinated with boolean connector panels via event system 
    private int selectionMode;
    
    private SingleNodeConstraintPanel markedTreeNode;
    
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
        rootConstPanel.setSynchronized(true);
        treePanel.add(rootConstPanel);
        
        JScrollPane treeScroll = new JScrollPane(treePanel);
        add(treeScroll);  
        
        children = new HashMap<SingleNodeConstraintPanel, List<SingleNodeConstraintPanel>>();
        parents = new HashMap<SingleNodeConstraintPanel, SingleNodeConstraintPanel> ();
        
        selectionMode = -1;
        
        markedTreeNode = null;
    }
    
    public TreeFragmentPanel(KahinaController control, NodeConstraintOptions constrOptions)
    {
        this(control);
        this.constrOptions = constrOptions;
        rootConstPanel.setConstrOptions(constrOptions);
    }
    
    public void setRootConstraint(SingleNodeConstraintPanel root)
    {
        if (rootConstPanel != null)
        {
            treePanel.remove(rootConstPanel);
            rootConstPanel.setSynchronized(false);
            rootConstPanel = null;
        }
        rootConstPanel = root;
        rootConstPanel.setHintPanel(hintPanel);   
        treePanel.add(rootConstPanel);
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("negOperation"))
        {
            if (markedTreeNode.getMarkedPattern() != null)
            {
                markedTreeNode.introduceNegation(markedTreeNode.getMarkedPattern());
            }
            else
            {
                hint("Select first the constraint to be negated.", Color.RED);
            }
        }
        else if (s.equals("andOperation"))
        {
            if (markedTreeNode.getMarkedPattern() != null)
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
            if (markedTreeNode.getMarkedPattern() != null)
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
            if (markedTreeNode.getMarkedPattern() != null)
            {
                control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.CHANGE_NODE_SELECTION_MODE, BreakpointEditPanel.PENDING_IMPL_OPERATION));
                hint("Now select the consequent.", Color.BLACK);
            }
            else
            {
                hint("Antecedent must be selected before clicking this button.", Color.RED);
            }
        }
        else if (s.equals("addChild"))
        {
            if (markedTreeNode != null)
            {
                addNewChildNode(markedTreeNode);
                hint("Manipulate the tree structure or define complex node constraints.", Color.BLACK);
                control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.TREE_PATTERN_CHANGE));
            }
            else
            {
                hint("First select the tree node to which you want to add a new child.", Color.RED);
            }
        }
        else if (s.equals("removeNode"))
        {
            if (markedTreeNode == null)
            {
                hint("First select the tree node you want to remove.", Color.RED);
            }
            else if (markedTreeNode == rootConstPanel)
            {
                hint("Cannot remove the root node.", Color.RED);
            }
            else
            {
                removeMarkedNode();
                hint("Manipulate the tree structure or define complex node constraints.", Color.BLACK);
                control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.TREE_PATTERN_CHANGE));
            }
        }
    }
    
    private void addNewChildNode(SingleNodeConstraintPanel parent)
    {
        SingleNodeConstraintPanel child = new SingleNodeConstraintPanel(constrOptions, control);
        child.setHintPanel(hintPanel);
        List<SingleNodeConstraintPanel> nodeChildren = children.get(parent);
        if (nodeChildren == null)
        {
            nodeChildren = new ArrayList<SingleNodeConstraintPanel>();
            children.put(parent, nodeChildren);
        }
        nodeChildren.add(child);
        parents.put(child, parent);
        treePanel.add(child);
        treePanel.recalculateCoordinates();
    }
    
    private void removeMarkedNode()
    {
        children.get(parents.get(markedTreeNode)).remove(markedTreeNode);
        parents.remove(markedTreeNode);
        treePanel.remove(markedTreeNode);
        markedTreeNode = null;
        treePanel.recalculateCoordinates();
    }
    
    public SingleNodeConstraintPanel getRoot()
    {
        return rootConstPanel;
    }
   
    public SingleNodeConstraintPanel getParent(JPanel node)
    {
        return parents.get(node);
    }
    
    public List<SingleNodeConstraintPanel> getChildren(JPanel node)
    {
        List<SingleNodeConstraintPanel> nodeChildren = children.get(node);
        if (nodeChildren == null)
        {
            nodeChildren = new ArrayList<SingleNodeConstraintPanel>();
        }
        return nodeChildren;
    } 
    
    public void hint(String hint)
    {
        hintPanel.hint(hint);
    }
    
    public void hint(String hint, Color color)
    {
        hintPanel.hint(hint,color);
    }
    
    @Override
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
            rootConstPanel.setSynchronized(true);
            treePanel.add(rootConstPanel);
        }
        hintPanel.setEnabled(true);
        boolOpsPanel.setEnabled(true);
        nodeOpsPanel.setEnabled(true);
        treePanel.recalculateCoordinates();
        validate();
    }
    
    public void deactivateAllComponents()
    {
        clear();
        hintPanel.setEnabled(false);
        boolOpsPanel.setEnabled(false);
        nodeOpsPanel.setEnabled(false);
        treePanel.recalculateCoordinates();
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
        else if (event.getEditorEventType() == BreakpointEditorEvent.TREE_NODE_UPDATE)
        {
            treePanel.recalculateCoordinates();
            if (markedTreeNode != null)
            {
                if (markedTreeNode != event.getPanel())
                {
                    markedTreeNode.removeAllMarkings();
                }
            }        
            markedTreeNode = event.getPanel();
            if (markedTreeNode != null)
            {
                markedTreeNode.setMarked(true);
            }
        }
        else if (event.getEditorEventType() == BreakpointEditorEvent.SYNCHRONIZE_EDITOR_VIEWS)
        {
           if (event.getPanel() != rootConstPanel && rootConstPanel != null)
           {
               rootConstPanel.takeOverStructure(event.getPanel());
           }
        }
    }
    
    public TreePattern getTreePattern()
    {
        TreePattern treePattern = new TreePattern();
        treePattern.setRoot(buildTreePatternNode(rootConstPanel));
        return treePattern;
    }
    
    private TreePatternNode buildTreePatternNode(SingleNodeConstraintPanel panel)
    {
        TreePatternNode node = new TreePatternNode(panel.getNodeConstraint());
        List<SingleNodeConstraintPanel> childNodes = children.get(panel);
        if (childNodes != null)
        {
            for (SingleNodeConstraintPanel child : childNodes)
            {
                node.getChildren().add(buildTreePatternNode(child));
            }
        }
        return node;
    }
    
    public void clear()
    {
        if (rootConstPanel != null)
        {
            clearNode(rootConstPanel);  
            rootConstPanel = null;
        }
        
        children = new HashMap<SingleNodeConstraintPanel, List<SingleNodeConstraintPanel>>();
        parents = new HashMap<SingleNodeConstraintPanel, SingleNodeConstraintPanel> ();
        
        selectionMode = -1;
        
        markedTreeNode = null;
    }
    
    private void clearNode(SingleNodeConstraintPanel panel)
    {
        treePanel.remove(panel);
        for (SingleNodeConstraintPanel child : getChildren(panel))
        {
            clearNode(child);
        }
    }
    
    public void displayTreePattern(TreePattern pat)
    {
        rootConstPanel = new SingleNodeConstraintPanel(constrOptions, control, pat.getRoot());
        displaySubtreePattern(pat.getRoot(), rootConstPanel);
        rootConstPanel.setHintPanel(hintPanel);
        rootConstPanel.setSynchronized(true);
        treePanel.add(rootConstPanel);
        treePanel.recalculateCoordinates();
        validate();
    }
    
    private void displaySubtreePattern(TreePatternNode node, SingleNodeConstraintPanel parent)
    {    
        if (node.getChildren() != null)
        {
            for (TreePatternNode child : node.getChildren())
            {
                SingleNodeConstraintPanel childPanel = new SingleNodeConstraintPanel(constrOptions, control, child);
                childPanel.setHintPanel(hintPanel);
                List<SingleNodeConstraintPanel> nodeChildren = children.get(parent);
                if (nodeChildren == null)
                {
                    nodeChildren = new ArrayList<SingleNodeConstraintPanel>();
                    children.put(parent, nodeChildren);
                }
                nodeChildren.add(childPanel);
                parents.put(childPanel, parent);
                treePanel.add(childPanel);
                
                displaySubtreePattern(child, childPanel);
            }
        }
    }
}
