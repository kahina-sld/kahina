package org.kahina.gui.breakpoint;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
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

import org.kahina.breakpoint.TreeNodePattern;

public class NodeConstraintPanel extends JPanel implements ActionListener
{
    NodeConstraintOptions constrOptions;
    int elementaryConstraintNumber;
    
    //data structures for the construction of the node pattern
    List<TreeNodePattern> basePatterns;
    private TreeNodePattern virtualRootPattern;
    Map<TreeNodePattern,TreeNodePattern> parentPatterns;
    
    JPanel elConstPanel;
    BooleanConnectorPanel boolPanel;
    List<JComboBox> typeComboBoxes;
    List<JComboBox> relComboBoxes;
    //can be text fields or combo boxes
    List<JComboBox> valComboBoxes;
    List<JButton> addButtons;
    List<JButton> remButtons;
    
    BooleanOperationsPanel boolOpsPanel;
    NodeOperationsPanel nodeOpsPanel;
    BreakpointEditorHintPanel hintPanel;
    
    //store internally which kind of connective is being built 
    public int selectionMode;
    
    public static final int NO_PENDING_OPERATION = -1;
    public static final int PENDING_AND_OPERATION = 0;
    public static final int PENDING_OR_OPERATION = 1;
    public static final int PENDING_IMPL_OPERATION = 2;
    
    public NodeConstraintPanel()
    {
        constrOptions = new NodeConstraintOptions();
        constrOptions.setStandardOptions();
        
        elementaryConstraintNumber = 0;
        
        basePatterns = new ArrayList<TreeNodePattern>();
        parentPatterns = new HashMap<TreeNodePattern,TreeNodePattern>();
        virtualRootPattern = new TreeNodePattern();
        
        setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
        
        elConstPanel = new JPanel();
        elConstPanel.setLayout(new GridBagLayout());
        elConstPanel.setBorder(BorderFactory.createTitledBorder("Elementary Constraints")); 
        
        typeComboBoxes = new ArrayList<JComboBox>();
        relComboBoxes = new ArrayList<JComboBox>();
        valComboBoxes = new ArrayList<JComboBox>();
        
        addButtons = new ArrayList<JButton>();
        remButtons = new ArrayList<JButton>();
        
        boolPanel = new BooleanConnectorPanel(this);
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridx = 0;
        c.gridy = 0;
        c.gridheight = 1;
        elConstPanel.add(boolPanel, c);
        
        addElementaryConstraint(0);
        
        add(elConstPanel);
        
        JPanel bottomPanel = new JPanel();
        bottomPanel.setLayout(new BoxLayout(bottomPanel, BoxLayout.LINE_AXIS));
        
        boolOpsPanel = new BooleanOperationsPanel(this);           
        bottomPanel.add(boolOpsPanel);
        nodeOpsPanel = new NodeOperationsPanel(this);           
        bottomPanel.add(nodeOpsPanel);      
        add(bottomPanel);
        
        hintPanel = new BreakpointEditorHintPanel();   
        add(hintPanel);
        
        selectionMode = -1;
    }
    
    public NodeConstraintPanel(NodeConstraintOptions constrOptions)
    {
        this();
        this.constrOptions = constrOptions;
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
        if (s.startsWith("changeType"))
        {
            Integer rowID = Integer.parseInt(s.substring(10));
            String type = typeComboBoxes.get(rowID).getSelectedItem().toString();
            relComboBoxes.get(rowID).setModel(new DefaultComboBoxModel(constrOptions.getRelationsForType(type).toArray()));
            basePatterns.get(rowID).setType(type);
            hint("Complete the node constraint by selecting a relation and/or a value.");
        }
        else if (s.startsWith("changeRel"))
        {
            Integer rowID = Integer.parseInt(s.substring(9));
            String rel = relComboBoxes.get(rowID).getSelectedItem().toString();
            basePatterns.get(rowID).setRelation(rel);
            hint("Complete the node constraint by specifying a value.");
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
        else if (s.equals("negOperation"))
        {
            if (boolPanel.markedPattern != null)
            {
                introduceNegation(boolPanel.markedPattern);
            }
            else
            {
                hint("Select first the constraint to be negated.", Color.RED);
            }
        }
        else if (s.equals("andOperation"))
        {
            if (boolPanel.markedPattern != null)
            {
                selectionMode = PENDING_AND_OPERATION;
                hint("Now select the second conjunct.", Color.BLACK);
            }
            else
            {
                hint("First conjunct must be selected before clicking this button.", Color.RED);
            }
        }
        else if (s.equals("orOperation"))
        {
            if (boolPanel.markedPattern != null)
            {
                selectionMode = PENDING_OR_OPERATION;
                hint("Now select the second disjunct.", Color.BLACK);
            }
            else
            {
                hint("First disjunct must be selected before clicking this button.", Color.RED);
            }
        }
        else if (s.equals("implOperation"))
        {
            if (boolPanel.markedPattern != null)
            {
                selectionMode = PENDING_IMPL_OPERATION;
                hint("Now select the consequent.", Color.BLACK);
            }
            else
            {
                hint("Antecedent must be selected before clicking this button.", Color.RED);
            }
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
        while (rightAncestor == null && rightAncestor != getRootPattern())
        {
            System.err.println(rightAncestor.toString());
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
        System.err.println(getRootPattern().toString());
        boolPanel.recalculateCoordinates();
        boolPanel.adaptSize();     
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
        elConstPanel.setEnabled(true);
        boolPanel.setEnabled(true);
        hintPanel.setEnabled(true);
        boolOpsPanel.setEnabled(true);
        nodeOpsPanel.setEnabled(true);
    }
    
    public void deactivateAllComponents()
    {
        elConstPanel.setEnabled(false);
        boolPanel.setEnabled(false);
        hintPanel.setEnabled(false);
        boolOpsPanel.setEnabled(false);
        nodeOpsPanel.setEnabled(false);
    }
}
