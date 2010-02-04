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
    
    List<TreeNodePattern> basePatterns;
    Set<TreeNodePattern> complexPatterns;
    TreeNodePattern rootPattern;
    Map<TreeNodePattern,TreeNodePattern> parentPatterns;
    
    JPanel elConstPanel;
    BooleanConnectorPanel boolPanel;
    List<JComboBox> typeComboBoxes;
    List<JComboBox> relComboBoxes;
    //can be text fields or combo boxes
    List<JComboBox> valComboBoxes;
    List<JButton> addButtons;
    List<JButton> remButtons;
    
    JLabel hintLabel;
    
    public NodeConstraintPanel()
    {
        constrOptions = new NodeConstraintOptions();
        constrOptions.setStandardOptions();
        
        elementaryConstraintNumber = 0;
        
        basePatterns = new ArrayList<TreeNodePattern>();
        complexPatterns = new HashSet<TreeNodePattern>();
        parentPatterns = new HashMap<TreeNodePattern,TreeNodePattern>();
        
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
        
        JPanel boolOpsPanel = new JPanel();
        boolOpsPanel.setLayout(new BoxLayout(boolOpsPanel, BoxLayout.LINE_AXIS));
        boolOpsPanel.setBorder(BorderFactory.createTitledBorder("Boolean Operations"));
        
        JButton andOperationButton = new JButton("&");
        andOperationButton.setActionCommand("andOperation");
        andOperationButton.addActionListener(this);
        boolOpsPanel.add(andOperationButton);
        boolOpsPanel.add(Box.createRigidArea(new Dimension(10,0)));
        
        JButton orOperationButton = new JButton("v");
        orOperationButton.setActionCommand("orOperation");
        orOperationButton.addActionListener(this);
        boolOpsPanel.add(orOperationButton);
        boolOpsPanel.add(Box.createRigidArea(new Dimension(10,0)));
        
        JButton negOperationButton = new JButton("~");
        negOperationButton.setActionCommand("negOperation");
        negOperationButton.addActionListener(this);
        boolOpsPanel.add(negOperationButton);
        boolOpsPanel.add(Box.createRigidArea(new Dimension(10,0)));
        
        JButton implOperationButton = new JButton("->");
        implOperationButton.setActionCommand("implOperation");
        implOperationButton.addActionListener(this);
        boolOpsPanel.add(implOperationButton);
        boolOpsPanel.add(Box.createRigidArea(new Dimension(10,0)));
        
        bottomPanel.add(boolOpsPanel);
        
        JPanel hintPanel = new JPanel();
        hintPanel.setBorder(BorderFactory.createTitledBorder("Hint"));  
        hintLabel = new JLabel("Define a node constraint by selecting a type.");
        hintPanel.add(hintLabel);
        
        bottomPanel.add(hintPanel);
        
        add(bottomPanel);
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
    
    public TreeNodePattern getNodeConstraint()
    {
        return new TreeNodePattern();
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
            hint("Complete the node constraint by selecting a relation and/or a value.");
        }
        else if (s.startsWith("changeRel"))
        {
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
    }
    
    private void introduceNegation(TreeNodePattern argument)
    {
        TreeNodePattern neg = new TreeNodePattern(TreeNodePattern.NEGATION, argument);     
        complexPatterns.add(neg);
        parentPatterns.put(neg, parentPatterns.get(argument));
        if (argument == rootPattern)
        {
            rootPattern = neg;
        }
        else
        {
            TreeNodePattern parent = parentPatterns.get(argument);
            if (argument == parent.getLeftArgument())
            {
                parent.setLeftArgument(neg);
            }
            else
            {
                parent.setRightArgument(neg);
            }
        }
        boolPanel.recalculateCoordinates();
        boolPanel.adaptSize();     
        revalidate();
        repaint();
    }
    
    public void addElementaryConstraint(int rowID)
    {
        generateElementaryConstraintRow(rowID);
        
        adaptNamingAndLayout(rowID);
       
        elementaryConstraintNumber++;
        adaptBoolPanel();
        basePatterns.add(rowID, new TreeNodePattern());
        if (rootPattern == null)
        {
            rootPattern = basePatterns.get(rowID);
        }
        else
        {
            TreeNodePattern newRoot = new TreeNodePattern(TreeNodePattern.CONJUNCTION, rootPattern, basePatterns.get(rowID));
            complexPatterns.add(newRoot);
            parentPatterns.put(rootPattern, newRoot);
            parentPatterns.put(basePatterns.get(rowID), newRoot);
            rootPattern = newRoot;
        }
        boolPanel.recalculateCoordinates();
        boolPanel.adaptSize();     
        revalidate();
        repaint();
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
        basePatterns.remove(rowID);

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
        hintLabel.setForeground(Color.BLACK);
        hintLabel.setText(hint);
    }
    
    public void hint(String hint, Color color)
    {
        hintLabel.setForeground(color);
        hintLabel.setText(hint);
    }
}
