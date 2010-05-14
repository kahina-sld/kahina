package org.kahina.core.gui.breakpoint;

import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
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
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.border.Border;
import javax.swing.text.JTextComponent;

import org.kahina.core.breakpoint.PatternFormatException;
import org.kahina.core.breakpoint.TreeNodePattern;
import org.kahina.core.breakpoint.TreePatternNode;
import org.kahina.core.control.KahinaController;

public class SingleNodeConstraintPanel extends JPanel implements ActionListener, MouseListener
{
    private KahinaController control;
    
    private NodeConstraintOptions constrOptions;
    private int elementaryConstraintNumber;
    
    //  data structures for the construction of the node pattern
    private List<TreeNodePattern> basePatterns;
    private TreeNodePattern virtualRootPattern;
    private Map<TreeNodePattern,TreeNodePattern> parentPatterns;
    
    private JPanel elConstPanel;
    private BooleanConnectorPanel boolPanel;
    private List<JComboBox> typeComboBoxes;
    private List<JComboBox> relComboBoxes;
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
        
        basePatterns = new ArrayList<TreeNodePattern>();
        parentPatterns = new HashMap<TreeNodePattern,TreeNodePattern>();
        virtualRootPattern = new TreeNodePattern();
        
        elConstPanel = new JPanel();
        elConstPanel.setLayout(new GridBagLayout());
        elConstPanel.setBorder(BorderFactory.createTitledBorder("Node Constraint")); 
        
        typeComboBoxes = new ArrayList<JComboBox>();
        relComboBoxes = new ArrayList<JComboBox>();
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
        this.add(elConstPanel);
    }
    
    public SingleNodeConstraintPanel(NodeConstraintOptions constrOptions, KahinaController control, TreePatternNode patternNode)
    {
        this.control = control;
        this.addMouseListener(this);
        
        this.constrOptions = constrOptions;     
        elementaryConstraintNumber = 0;
        
        basePatterns = new ArrayList<TreeNodePattern>();
        parentPatterns = new HashMap<TreeNodePattern,TreeNodePattern>();
        virtualRootPattern = new TreeNodePattern();
        
        elConstPanel = new JPanel();
        elConstPanel.setLayout(new GridBagLayout());
        elConstPanel.setBorder(BorderFactory.createTitledBorder("Node Constraint")); 
        
        typeComboBoxes = new ArrayList<JComboBox>();
        relComboBoxes = new ArrayList<JComboBox>();
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
        
        setRootPattern(patternNode.getPattern());  
        addStructure(getRootPattern());

        this.add(elConstPanel);
        displayChangeInConnectiveStructure();
    }
    
    private void addStructure(TreeNodePattern node)
    {
        TreeNodePattern left = node.getLeftArgument();
        TreeNodePattern right = node.getRightArgument();
        if (left == null)
        {
            generateElementaryConstraintRow(elementaryConstraintNumber);        
            adaptNamingAndLayout(elementaryConstraintNumber);           
            basePatterns.add(node);
            typeComboBoxes.get(elementaryConstraintNumber).setSelectedItem(node.getTypeAsString());
            relComboBoxes.get(elementaryConstraintNumber).setModel(new DefaultComboBoxModel(constrOptions.getRelationsForType(node.getTypeAsString()).toArray()));
            relComboBoxes.get(elementaryConstraintNumber).setSelectedItem(node.getRelAsString());
            valComboBoxes.get(elementaryConstraintNumber).setSelectedItem(node.getValueAsString());
            elementaryConstraintNumber++;
            adaptBoolPanel();
        }
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
        
        basePatterns = new ArrayList<TreeNodePattern>();
        parentPatterns = new HashMap<TreeNodePattern,TreeNodePattern>();
        virtualRootPattern = new TreeNodePattern();
        
        elConstPanel.removeAll();
        
        typeComboBoxes = new ArrayList<JComboBox>();
        relComboBoxes = new ArrayList<JComboBox>();
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
    
    public TreeNodePattern getMarkedPattern()
    {
        return boolPanel.getMarkedPattern();
    }

    public List<TreeNodePattern> getBasePatterns()
    {
        return basePatterns;
    }

    private void generateElementaryConstraintRow(int rowID)
    {
        GridBagConstraints c = new GridBagConstraints();

        JComboBox constTypeChoice = new JComboBox(constrOptions.getTypes().toArray());
        constTypeChoice.setActionCommand("changeType" + rowID);      
        constTypeChoice.addActionListener(this);      
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridx = 1;
        c.gridy = rowID;
        elConstPanel.add(constTypeChoice, c);
        typeComboBoxes.add(rowID, constTypeChoice);

        JComboBox compTypeChoice = new JComboBox(constrOptions.getRelationsForType("--").toArray());
        compTypeChoice.setActionCommand("changeRel" + rowID);    
        compTypeChoice.addActionListener(this);  
        c.gridx = 2;
        elConstPanel.add(compTypeChoice, c);
        relComboBoxes.add(rowID, compTypeChoice);
         
        List<String> values = constrOptions.getValuesForType("--");
        JComboBox valueChoice = new JComboBox(values.toArray());
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
        c.gridx = 4;
        elConstPanel.add(addConstButton, c);
        addButtons.add(rowID, addConstButton);
        
        JButton remConstButton = new JButton("x");
        remConstButton.setForeground(Color.RED);
        if (rowID == 0) remConstButton.setEnabled(false);
        remConstButton.setActionCommand("remConst" + rowID);    
        remConstButton.addActionListener(this);  
        c.gridx = 5;
        elConstPanel.add(remConstButton, c);
        remButtons.add(rowID, remConstButton);
    }
    
    public void setRootPattern(TreeNodePattern newRoot)
    {
        virtualRootPattern.setLeftArgument(newRoot);
        parentPatterns.put(newRoot, virtualRootPattern);
    }
    
    public TreeNodePattern getRootPattern()
    {
        return virtualRootPattern.getLeftArgument();
    }
    
    public TreeNodePattern getNodeConstraint()
    {
        return getRootPattern();
    }
    
    public void setNodeConstraint(TreeNodePattern e)
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
    
    public void introduceNegation(TreeNodePattern argument)
    {
        TreeNodePattern neg = new TreeNodePattern(TreeNodePattern.NEGATION, argument); 
        TreeNodePattern parent = parentPatterns.get(argument);
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
    
    public boolean introduceConjunction(TreeNodePattern arg1, TreeNodePattern arg2)
    {
        if (!consistencyCheck(arg1,arg2)) return false;
        //build the new conjunct node
        TreeNodePattern conj = new TreeNodePattern(TreeNodePattern.CONJUNCTION, arg1, arg2);     
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
            TreeNodePattern newRootPattern = new TreeNodePattern(TreeNodePattern.CONJUNCTION, conj, getRootPattern());
            parentPatterns.put(getRootPattern(), newRootPattern);
            parentPatterns.put(conj, newRootPattern);
            setRootPattern(newRootPattern); 
        }
        
        boolPanel.markedPattern = conj;
        displayChangeInConnectiveStructure();
        return true;
    }
    
    public boolean introduceDisjunction(TreeNodePattern arg1, TreeNodePattern arg2)
    {
        if (!consistencyCheck(arg1,arg2)) return false;
        //build the new conjunct node
        TreeNodePattern disj = new TreeNodePattern(TreeNodePattern.DISJUNCTION, arg1, arg2);     
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
            TreeNodePattern newRootPattern = new TreeNodePattern(TreeNodePattern.CONJUNCTION, disj, getRootPattern());
            parentPatterns.put(getRootPattern(), newRootPattern);
            parentPatterns.put(disj, newRootPattern);
            setRootPattern(newRootPattern); 
        }
        
        boolPanel.markedPattern = disj;
        displayChangeInConnectiveStructure();
        return true;
    }
    
    public boolean introduceImplication(TreeNodePattern arg1, TreeNodePattern arg2)
    {
        if (!consistencyCheck(arg1,arg2)) return false;
        //build the new conjunct node
        TreeNodePattern impl = new TreeNodePattern(TreeNodePattern.IMPLICATION, arg1, arg2);     
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
            TreeNodePattern newRootPattern = new TreeNodePattern(TreeNodePattern.CONJUNCTION, impl, getRootPattern());
            parentPatterns.put(getRootPattern(), newRootPattern);
            parentPatterns.put(impl, newRootPattern);
            setRootPattern(newRootPattern); 
        }
        
        boolPanel.markedPattern = impl;
        displayChangeInConnectiveStructure();
        return true;
    }
    
    private boolean consistencyCheck(TreeNodePattern arg1, TreeNodePattern arg2)
    {
        //consistency check: neither node must dominate the other
        TreeNodePattern leftAncestor = arg1;
        if (leftAncestor == arg2) return false;
        while (leftAncestor != getRootPattern())
        {
            leftAncestor = parentPatterns.get(leftAncestor);
            if (leftAncestor == arg2) return false;
        }
        TreeNodePattern rightAncestor = arg2;
        if (rightAncestor == arg1) return false;
        while (rightAncestor != null && rightAncestor != getRootPattern())
        {
            rightAncestor = parentPatterns.get(rightAncestor);
            if (rightAncestor == arg1) return false;
        }
        return true;
    }
    
    private void rebalanceStructureForConnective(TreeNodePattern arg1, TreeNodePattern arg2)
    {
        //determine the "loose ends" and rebalance the structure
        removeFromStructureWithoutBreakingIt(arg1);
        removeFromStructureWithoutBreakingIt(arg2);
    }
    
    private void removeFromStructureWithoutBreakingIt(TreeNodePattern arg1)
    {
        if (getRootPattern() == arg1 && arg1.getLeftArgument() == null)
        {
            virtualRootPattern = new TreeNodePattern();
            parentPatterns.remove(arg1);
            return;
        }
        TreeNodePattern node = arg1;
        TreeNodePattern parent = parentPatterns.get(arg1);
        TreeNodePattern grandparent = parentPatterns.get(parent);
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
    
    private void replaceChild(TreeNodePattern parent, TreeNodePattern child, TreeNodePattern newChild)
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
        basePatterns.add(rowID, new TreeNodePattern());
        if (getRootPattern() == null)
        {
            setRootPattern(basePatterns.get(rowID));
        }
        else
        {
            TreeNodePattern newRoot = new TreeNodePattern(TreeNodePattern.CONJUNCTION, getRootPattern(), basePatterns.get(rowID));
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
    
    public String toString()
    {
        return getRootPattern().toString();
    }
}
