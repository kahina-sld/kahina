package org.kahina.core.edit.breakpoint;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.border.Border;
import javax.swing.text.JTextComponent;

import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaSimpleProperty;
import org.kahina.core.data.agent.patterns.PatternFormatException;
import org.kahina.core.data.agent.patterns.TreePatternNode;

public class SingleNodeConstraintPanel extends JPanel implements ActionListener, MouseListener
{
    private KahinaController control;
    
    private NodeConstraintOptions constrOptions;
    private int elementaryConstraintNumber;
    
    //data structures for the construction of the node pattern
    private List<KahinaSimpleProperty> basePatterns;
    private KahinaSimpleProperty virtualRootPattern;
    private Map<KahinaSimpleProperty,KahinaSimpleProperty> parentPatterns;
    
    private JPanel elConstPanel;
    private BooleanConnectorPanel boolPanel;
    private List<NodeConstraintComboBox> typeComboBoxes;
    private List<NodeConstraintComboBox> relComboBoxes;
    private List<JComboBox> valComboBoxes;
    private List<ValueBoxKeyListener> valKeyListeners;
    private List<JButton> addButtons;
    private List<JButton> remButtons;
    
    //link to an external hint panel to display hints and error messages
    private BreakpointEditorHintPanel hintPanel;
    
    //needed for synchronization between node constraint and tree fragment panel
    private boolean synchronizationMode = false;
    private boolean isSynchronized = false;
    
    public SingleNodeConstraintPanel(NodeConstraintOptions constrOptions, KahinaController control)
    {
        this.control = control;
        this.addMouseListener(this);
        
        this.constrOptions = constrOptions;     
        elementaryConstraintNumber = 0;
        
        basePatterns = new ArrayList<KahinaSimpleProperty>();
        parentPatterns = new HashMap<KahinaSimpleProperty,KahinaSimpleProperty>();
        virtualRootPattern = new KahinaSimpleProperty();
        
        this.setLayout(new BoxLayout(this,BoxLayout.PAGE_AXIS));
        
        this.add(Box.createRigidArea(new Dimension(0,0)));
        
        elConstPanel = new JPanel();
        elConstPanel.setLayout(new GridBagLayout());
        elConstPanel.setBorder(BorderFactory.createTitledBorder("Node Constraint")); 
        
        typeComboBoxes = new ArrayList<NodeConstraintComboBox>();
        relComboBoxes = new ArrayList<NodeConstraintComboBox>();
        valComboBoxes = new ArrayList<JComboBox>();
        valKeyListeners = new ArrayList<ValueBoxKeyListener>();
        
        addButtons = new ArrayList<JButton>();
        remButtons = new ArrayList<JButton>();

        boolPanel = new BooleanConnectorPanel(this, control);
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridx = 0;
        c.gridy = 0;
        c.gridheight = 1;
        elConstPanel.add(boolPanel, c);
        
        addElementaryConstraint(0); 
        this.add(elConstPanel, Component.LEFT_ALIGNMENT);
    }
    
    public SingleNodeConstraintPanel(NodeConstraintOptions constrOptions, KahinaController control, KahinaSimpleProperty pattern)
    {
        this.control = control;
        this.addMouseListener(this);
        
        this.constrOptions = constrOptions;     
        elementaryConstraintNumber = 0;
        
        basePatterns = new ArrayList<KahinaSimpleProperty>();
        parentPatterns = new HashMap<KahinaSimpleProperty,KahinaSimpleProperty>();
        virtualRootPattern = new KahinaSimpleProperty();
        
        this.setLayout(new BoxLayout(this,BoxLayout.PAGE_AXIS));
        
        this.add(Box.createRigidArea(new Dimension(0,0)));
        
        elConstPanel = new JPanel();
        elConstPanel.setLayout(new GridBagLayout());
        elConstPanel.setBorder(BorderFactory.createTitledBorder("Pattern")); 
        
        typeComboBoxes = new ArrayList<NodeConstraintComboBox>();
        relComboBoxes = new ArrayList<NodeConstraintComboBox>();
        valComboBoxes = new ArrayList<JComboBox>();
        valKeyListeners = new ArrayList<ValueBoxKeyListener>();
        
        addButtons = new ArrayList<JButton>();
        remButtons = new ArrayList<JButton>();

        boolPanel = new BooleanConnectorPanel(this, control);
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridx = 0;
        c.gridy = 0;
        c.gridheight = 1;
        elConstPanel.add(boolPanel, c);
        
        setRootPattern(pattern); 
        //synchronization mode to prevent combo boxes from changing the pattern during construction
        synchronizationMode = true;
        addStructure(getRootPattern());
        synchronizationMode = false;

        this.add(elConstPanel, Component.LEFT_ALIGNMENT);
        displayChangeInConnectiveStructure();
    }
    
    private void addStructure(KahinaSimpleProperty node)
    {
        KahinaSimpleProperty left = node.getLeftArgument();
        KahinaSimpleProperty right = node.getRightArgument();
        //if we have a leaf pattern, add an elementary constraint row for it
        if (left == null)
        {
            generateElementaryConstraintRow(elementaryConstraintNumber);        
            adaptNamingAndLayout(elementaryConstraintNumber);           
            basePatterns.add(node);
            //System.err.println("displaying TreeNodePattern " + node.hashCode() + ": " + node);
            typeComboBoxes.get(elementaryConstraintNumber).setSelectedItem(node.getTypeAsString());
            relComboBoxes.get(elementaryConstraintNumber).setModel(constrOptions.getRelationsForType(node.getTypeAsString()));
            relComboBoxes.get(elementaryConstraintNumber).setSelectedItem(node.getRelAsString());
            valComboBoxes.get(elementaryConstraintNumber).setSelectedItem(node.getValueAsString());
            elementaryConstraintNumber++;
            adaptBoolPanel();
        }
        //boolean connectives do not require elementary constraint rows
        else
        {
            parentPatterns.put(left, node);
            addStructure(left);
            if (right != null)
            {
                parentPatterns.put(right, node);
                addStructure(right);
            }
        }
    }
    
    public void takeOverStructure(SingleNodeConstraintPanel panel)
    {    
        synchronizationMode = true;
        if (panel == null) return;
        elementaryConstraintNumber = 0;
        
        basePatterns = new ArrayList<KahinaSimpleProperty>();
        parentPatterns = new HashMap<KahinaSimpleProperty,KahinaSimpleProperty>();
        virtualRootPattern = new KahinaSimpleProperty();
        
        elConstPanel.removeAll();
        
        typeComboBoxes = new ArrayList<NodeConstraintComboBox>();
        relComboBoxes = new ArrayList<NodeConstraintComboBox>();
        valComboBoxes = new ArrayList<JComboBox>();
        valKeyListeners = new ArrayList<ValueBoxKeyListener>();
        
        addButtons = new ArrayList<JButton>();
        remButtons = new ArrayList<JButton>();

        boolPanel = new BooleanConnectorPanel(this, control);
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridx = 0;
        c.gridy = 0;
        c.gridheight = 1;
        elConstPanel.add(boolPanel, c);
        
        setRootPattern(panel.getRootPattern());  
        addStructure(getRootPattern());

        this.add(elConstPanel);
        displayChangeInConnectiveStructure();
        synchronizationMode = false;
    }
    
    public void setHintPanel(BreakpointEditorHintPanel hintPanel)
    {
        this.hintPanel = hintPanel;
    }
    
    public void setSynchronized(boolean sync)
    {
        this.isSynchronized = sync;
    }
    
    public NodeConstraintOptions getConstrOptions()
    {
        return constrOptions;
    }

    public void setConstrOptions(NodeConstraintOptions constrOptions)
    {
        this.constrOptions = constrOptions;
    }
    
    public KahinaSimpleProperty getMarkedPattern()
    {
        return boolPanel.getMarkedPattern();
    }

    public List<KahinaSimpleProperty> getBasePatterns()
    {
        return basePatterns;
    }

    private void generateElementaryConstraintRow(int rowID)
    {
        GridBagConstraints c = new GridBagConstraints();

        NodeConstraintComboBox constTypeChoice = new NodeConstraintComboBox(constrOptions.getTypes());
        constTypeChoice.setActionCommand("changeType" + rowID);      
        constTypeChoice.addActionListener(this);      
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridx = 1;
        c.gridy = rowID;
        elConstPanel.add(constTypeChoice, c);
        typeComboBoxes.add(rowID, constTypeChoice);

        NodeConstraintComboBox compTypeChoice = new NodeConstraintComboBox(constrOptions.getRelationsForType("--"));
        compTypeChoice.setActionCommand("changeRel" + rowID);    
        compTypeChoice.addActionListener(this);  
        c.gridx = 2;
        elConstPanel.add(compTypeChoice, c);
        relComboBoxes.add(rowID, compTypeChoice);
         
        List<String> values = constrOptions.getValuesForType("--");
        NodeConstraintComboBox valueChoice = new NodeConstraintComboBox(values);
        if (values.contains(""))
        {
            valueChoice.setEditable(true);
        }
        valueChoice.setActionCommand("changeVal" + rowID);    
        valueChoice.addActionListener(this); 
        ValueBoxKeyListener lst = new ValueBoxKeyListener(rowID);
        valueChoice.getEditor().getEditorComponent().addKeyListener(lst);
        valKeyListeners.add(lst);
        c.gridx = 3;
        elConstPanel.add(valueChoice, c);
        valComboBoxes.add(rowID, valueChoice);
        
        JButton addConstButton = new JButton("+");
        addConstButton.setForeground(Color.GREEN);
        addConstButton.setActionCommand("addConst" + rowID);    
        addConstButton.addActionListener(this);  
        addConstButton.setMargin(new Insets(1,0,2,0));
        c.gridx = 4;
        elConstPanel.add(addConstButton, c);
        addButtons.add(rowID, addConstButton);
        
        JButton remConstButton = new JButton("x");
        remConstButton.setForeground(Color.RED);
        if (rowID == 0) remConstButton.setEnabled(false);
        remConstButton.setActionCommand("remConst" + rowID);    
        remConstButton.addActionListener(this);  
        remConstButton.setMargin(new Insets(1,1,2,1));
        c.gridx = 5;
        elConstPanel.add(remConstButton, c);
        remButtons.add(rowID, remConstButton);
    }
    
    public void setRootPattern(KahinaSimpleProperty newRoot)
    {
        virtualRootPattern.setLeftArgument(newRoot);
        parentPatterns.put(newRoot, virtualRootPattern);
    }
    
    public KahinaSimpleProperty getRootPattern()
    {
        return virtualRootPattern.getLeftArgument();
    }
    
    public KahinaSimpleProperty getNodeConstraint()
    {
        return getRootPattern();
    }
    
    public void setNodeConstraint(KahinaSimpleProperty e)
    {
        
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (!synchronizationMode)
        {
            if (s.startsWith("changeType"))
            {
    
                Integer rowID = Integer.parseInt(s.substring(10));
                String type = typeComboBoxes.get(rowID).getSelectedItem().toString();
                relComboBoxes.get(rowID).setModel(new DefaultComboBoxModel(constrOptions.getRelationsForType(type).toArray()));
                basePatterns.get(rowID).setType(type);
                basePatterns.get(rowID).setRelation(relComboBoxes.get(rowID).getSelectedItem().toString());
                hint("Complete the node constraint by selecting a relation and/or a value.");
            }
            else if (s.startsWith("changeRel"))
            {
                Integer rowID = Integer.parseInt(s.substring(9));
                String rel = relComboBoxes.get(rowID).getSelectedItem().toString();
                basePatterns.get(rowID).setRelation(rel);
                hint("Complete the node constraint by specifying a value.");
            }
            else if (s.startsWith("changeVal"))
            {
                Integer rowID = Integer.parseInt(s.substring(9));
                String val = valComboBoxes.get(rowID).getSelectedItem().toString();
                try
                {
                    basePatterns.get(rowID).parseValue(val);
                }
                catch (PatternFormatException formatError)
                {
                    hint(formatError.getMessage(), Color.RED);
                    valComboBoxes.get(rowID).setSelectedItem(basePatterns.get(rowID).getValueAsString());
                    return;
                }
                hint("Add a new atomic condition or create a new connective.");
            }
            else if (s.startsWith("addConst"))
            {
                Integer rowID = Integer.parseInt(s.substring(8));
                addElementaryConstraint(rowID + 1);
            }
            else if (s.startsWith("remConst"))
            {
                Integer rowID = Integer.parseInt(s.substring(8));
                removeElementaryConstraint(rowID);
            }
            announceUpdate();
            synchronizeEditors();
        }
    }
    
    public void introduceNegation(KahinaSimpleProperty argument)
    {
        KahinaSimpleProperty neg = new KahinaSimpleProperty(KahinaSimpleProperty.NEGATION, argument); 
        KahinaSimpleProperty parent = parentPatterns.get(argument);
        parentPatterns.put(neg,parent);
        parentPatterns.put(argument,neg);
        if (argument == parent.getLeftArgument())
        {
            parent.setLeftArgument(neg);
        }
        else
        {
            parent.setRightArgument(neg);
        } 
        
        boolPanel.markedPattern = neg;
        displayChangeInConnectiveStructure();
        announceUpdate();
        synchronizeEditors();
    }
    
    public boolean introduceConjunction(KahinaSimpleProperty arg1, KahinaSimpleProperty arg2)
    {
        if (!consistencyCheck(arg1,arg2)) return false;
        //build the new conjunct node
        KahinaSimpleProperty conj = new KahinaSimpleProperty(KahinaSimpleProperty.CONJUNCTION, arg1, arg2);     
        //determine the "loose ends" and rebalance the structure
        rebalanceStructureForConnective(arg1, arg2);
        //establish new structure, new root is necessary
        parentPatterns.put(arg1, conj);
        parentPatterns.put(arg2, conj);      
        if (getRootPattern() == null)
        {
            setRootPattern(conj); 
        }
        else
        {
            KahinaSimpleProperty newRootPattern = new KahinaSimpleProperty(KahinaSimpleProperty.CONJUNCTION, conj, getRootPattern());
            parentPatterns.put(getRootPattern(), newRootPattern);
            parentPatterns.put(conj, newRootPattern);
            setRootPattern(newRootPattern); 
        }
        
        boolPanel.markedPattern = conj;
        displayChangeInConnectiveStructure();
        return true;
    }
    
    public boolean introduceDisjunction(KahinaSimpleProperty arg1, KahinaSimpleProperty arg2)
    {
        if (!consistencyCheck(arg1,arg2)) return false;
        //build the new conjunct node
        KahinaSimpleProperty disj = new KahinaSimpleProperty(KahinaSimpleProperty.DISJUNCTION, arg1, arg2);     
        rebalanceStructureForConnective(arg1, arg2);
        //establish new structure, new root is necessary
        parentPatterns.put(arg1, disj);
        parentPatterns.put(arg2, disj);      
        if (getRootPattern() == null)
        {
            setRootPattern(disj); 
        }
        else
        {
            KahinaSimpleProperty newRootPattern = new KahinaSimpleProperty(KahinaSimpleProperty.CONJUNCTION, disj, getRootPattern());
            parentPatterns.put(getRootPattern(), newRootPattern);
            parentPatterns.put(disj, newRootPattern);
            setRootPattern(newRootPattern); 
        }
        
        boolPanel.markedPattern = disj;
        displayChangeInConnectiveStructure();
        return true;
    }
    
    public boolean introduceImplication(KahinaSimpleProperty arg1, KahinaSimpleProperty arg2)
    {
        if (!consistencyCheck(arg1,arg2)) return false;
        //build the new conjunct node
        KahinaSimpleProperty impl = new KahinaSimpleProperty(KahinaSimpleProperty.IMPLICATION, arg1, arg2);     
        rebalanceStructureForConnective(arg1, arg2);
        //establish new structure, new root is necessary
        parentPatterns.put(arg1, impl);
        parentPatterns.put(arg2, impl);      
        if (getRootPattern() == null)
        {
            setRootPattern(impl); 
        }
        else
        {
            KahinaSimpleProperty newRootPattern = new KahinaSimpleProperty(KahinaSimpleProperty.CONJUNCTION, impl, getRootPattern());
            parentPatterns.put(getRootPattern(), newRootPattern);
            parentPatterns.put(impl, newRootPattern);
            setRootPattern(newRootPattern); 
        }
        
        boolPanel.markedPattern = impl;
        displayChangeInConnectiveStructure();
        return true;
    }
    
    private boolean consistencyCheck(KahinaSimpleProperty arg1, KahinaSimpleProperty arg2)
    {
        //consistency check: neither node must dominate the other
        KahinaSimpleProperty leftAncestor = arg1;
        if (leftAncestor == arg2) return false;
        while (leftAncestor != getRootPattern())
        {
            leftAncestor = parentPatterns.get(leftAncestor);
            if (leftAncestor == arg2) return false;
        }
        KahinaSimpleProperty rightAncestor = arg2;
        if (rightAncestor == arg1) return false;
        while (rightAncestor != null && rightAncestor != getRootPattern())
        {
            rightAncestor = parentPatterns.get(rightAncestor);
            if (rightAncestor == arg1) return false;
        }
        return true;
    }
    
    private void rebalanceStructureForConnective(KahinaSimpleProperty arg1, KahinaSimpleProperty arg2)
    {
        //determine the "loose ends" and rebalance the structure
        removeFromStructureWithoutBreakingIt(arg1);
        removeFromStructureWithoutBreakingIt(arg2);
    }
    
    private void removeFromStructureWithoutBreakingIt(KahinaSimpleProperty arg1)
    {
        if (getRootPattern() == arg1 && arg1.getLeftArgument() == null)
        {
            virtualRootPattern = new KahinaSimpleProperty();
            parentPatterns.remove(arg1);
            return;
        }
        KahinaSimpleProperty node = arg1;
        KahinaSimpleProperty parent = parentPatterns.get(arg1);
        KahinaSimpleProperty grandparent = parentPatterns.get(parent);
        while (parent.getRightArgument() == null && parent != getRootPattern())
        {
            node = parent;
            parent = grandparent;
            grandparent = parentPatterns.get(grandparent);
        }
        if (node == parent.getLeftArgument())
        {
            replaceChild(grandparent, parent, parent.getRightArgument());
        }
        else
        {
            replaceChild(grandparent, parent, parent.getLeftArgument());
        }
    }
    
    private void replaceChild(KahinaSimpleProperty parent, KahinaSimpleProperty child, KahinaSimpleProperty newChild)
    {
        if (child == parent.getLeftArgument())
        {
            parent.setLeftArgument(newChild);
            parentPatterns.put(newChild, parent);
            parentPatterns.remove(child);
        }
        else if (child == parent.getRightArgument())
        {
            parent.setRightArgument(newChild);
            parentPatterns.put(newChild, parent);
            parentPatterns.remove(child);
        }
    }
    
    public void addElementaryConstraint(int rowID)
    {
        generateElementaryConstraintRow(rowID);
        
        adaptNamingAndLayout(rowID);
       
        elementaryConstraintNumber++;
        adaptBoolPanel();
        basePatterns.add(rowID, new KahinaSimpleProperty());
        if (getRootPattern() == null)
        {
            setRootPattern(basePatterns.get(rowID));
        }
        else
        {
            KahinaSimpleProperty newRoot = new KahinaSimpleProperty(KahinaSimpleProperty.CONJUNCTION, getRootPattern(), basePatterns.get(rowID));
            parentPatterns.put(getRootPattern(), newRoot);
            parentPatterns.put(basePatterns.get(rowID), newRoot);
            setRootPattern(newRoot);
        }     
        displayChangeInConnectiveStructure();
    }
    
    public void removeElementaryConstraint(int rowID)
    {
        JComboBox typeComboBox = typeComboBoxes.remove(rowID);
        JComboBox relComboBox = relComboBoxes.remove(rowID);
        JComboBox valComboBox = valComboBoxes.remove(rowID);
        valKeyListeners.remove(rowID);
        JButton addButton = addButtons.remove(rowID);
        JButton remButton = remButtons.remove(rowID);
        elConstPanel.remove(typeComboBox);
        elConstPanel.remove(relComboBox);
        elConstPanel.remove(valComboBox);
        elConstPanel.remove(remButton);
        elConstPanel.remove(addButton);
        
        adaptNamingAndLayout(rowID);
        
        elementaryConstraintNumber--;
        adaptBoolPanel();
        removeFromStructureWithoutBreakingIt(basePatterns.remove(rowID));

        boolPanel.recalculateCoordinates();
        boolPanel.adaptSize();
        revalidate();
        repaint();
    }
    
    private void displayChangeInConnectiveStructure()
    {
        boolPanel.recalculateCoordinates();
        boolPanel.adaptSize(); 
        boolPanel.informControl(new BreakpointEditorEvent(BreakpointEditorEvent.TREE_NODE_UPDATE, this));
        revalidate();
        repaint();
    }
    
    private void adaptNamingAndLayout(int startIndex)
    {
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        for (int i = startIndex; i < typeComboBoxes.size(); i++)
        {
            typeComboBoxes.get(i).setActionCommand("changeType" + i);
            relComboBoxes.get(i).setActionCommand("changeRel" + i);
            valComboBoxes.get(i).setActionCommand("changeVal" + i);
            valKeyListeners.get(i).setRowID(i);
            addButtons.get(i).setActionCommand("addConst" + i);
            remButtons.get(i).setActionCommand("remConst" + i);
            
            elConstPanel.remove(typeComboBoxes.get(i));
            elConstPanel.remove(relComboBoxes.get(i));
            elConstPanel.remove(valComboBoxes.get(i));
            elConstPanel.remove(addButtons.get(i));  
            elConstPanel.remove(remButtons.get(i));
            
            c.gridy = i;
            c.gridx = 1;
            elConstPanel.add(typeComboBoxes.get(i), c);
            c.gridx = 2;
            elConstPanel.add(relComboBoxes.get(i), c);
            c.gridx = 3;
            elConstPanel.add(valComboBoxes.get(i), c);
            c.gridx = 4;
            elConstPanel.add(addButtons.get(i), c);
            c.gridx = 5;
            elConstPanel.add(remButtons.get(i), c);  
        }
    }
    
    private void adaptBoolPanel()
    {
        elConstPanel.remove(boolPanel);
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridx = 0;
        c.gridy = 0;
        c.gridheight = elementaryConstraintNumber + 1;
        elConstPanel.add(boolPanel, c);
    }
    
    public void hint(String hint)
    {
        if (hintPanel != null)
        {
            hintPanel.hint(hint);
        }
    }
    
    public void hint(String hint, Color color)
    {
        if (hintPanel != null)
        {
            hintPanel.hint(hint,color);
        }
    }
    
    public void setMarked(boolean mark)
    {
        if (mark)
        {
            Border compound;
            Border redline = BorderFactory.createLineBorder(Color.RED);
            Border titleBorder = BorderFactory.createTitledBorder("Node Constraint");
            compound = BorderFactory.createCompoundBorder(redline, titleBorder);
            elConstPanel.setBorder(compound); 
        }
        else
        {
            elConstPanel.setBorder(BorderFactory.createTitledBorder("Node Constraint")); 
        }
    }
    
    public void removeAllMarkings()
    {
        setMarked(false);
        boolPanel.markedPattern = null;
    }
    
    public void mouseClicked(MouseEvent arg0)
    {
        control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.TREE_NODE_UPDATE, this));
    }
    
    public void mouseEntered(MouseEvent arg0)
    {
    }
    
    public void mouseExited(MouseEvent arg0)
    {
    }
    
    public void mousePressed(MouseEvent arg0)
    {
    }
    
    public void mouseReleased(MouseEvent arg0)
    {
    }
    
    public void setNodeSelectionMode(int selectionMode)
    {
        boolPanel.setNodeSelectionMode(selectionMode);
    }
    
    private void announceUpdate()
    {
        if (!synchronizationMode)
        {
            boolPanel.informControl(new BreakpointEditorEvent(BreakpointEditorEvent.TREE_NODE_UPDATE, this));
        }
    }
    
    private void synchronizeEditors()
    {
        if (isSynchronized && !synchronizationMode)
        {
            boolPanel.informControl(new BreakpointEditorEvent(BreakpointEditorEvent.SYNCHRONIZE_EDITOR_VIEWS, this));
        }
    }
    
    private class ValueBoxKeyListener implements KeyListener
    {
        int rowID;
        
        public ValueBoxKeyListener(int rowID)
        {
            this.rowID = rowID;
        }
        
        public void setRowID(int rowID)
        {
            this.rowID = rowID;
        }
        
        public void keyPressed(KeyEvent e) 
        {
        }

        public void keyReleased(KeyEvent e) 
        {
            //need to access the embedded text field to read in current combo box content at any time
            String val = ((JTextComponent) valComboBoxes.get(rowID).getEditor().getEditorComponent()).getText();
            try
            {
                basePatterns.get(rowID).parseValue(val);
            }
            catch (PatternFormatException formatError)
            {
                hint(formatError.getMessage(), Color.RED);
                return;
            }
            hint("Add a new atomic condition or create a new connective.");
            announceUpdate();
            synchronizeEditors();
        }

        public void keyTyped(KeyEvent e) 
        {

        }
    }
    
    @Override
	public String toString()
    {
        return getRootPattern().toString();
    }
}
