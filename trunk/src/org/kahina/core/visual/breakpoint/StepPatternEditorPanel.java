package org.kahina.core.visual.breakpoint;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;

import org.kahina.core.control.KahinaCodeLineProperty;
import org.kahina.core.control.KahinaSimpleProperty;
import org.kahina.core.control.KahinaStepProperty;
import org.kahina.core.control.KahinaStepPropertySensor;
import org.kahina.core.control.KahinaSimplePropertySensor;
import org.kahina.core.data.breakpoint.patterns.TreePatternNode;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.edit.breakpoint.BreakpointEditorEvent;
import org.kahina.core.edit.breakpoint.BreakpointEditorHintPanel;
import org.kahina.core.edit.breakpoint.KahinaBreakpointEditorPanel;
import org.kahina.core.edit.breakpoint.NodeConstraintOptions;
import org.kahina.core.edit.breakpoint.SingleNodeConstraintPanel;

public class StepPatternEditorPanel extends JPanel implements ActionListener
{   
    JButton andOperationButton;
    JButton orOperationButton;
    JButton negOperationButton;
    JButton implOperationButton;
    
    private BreakpointEditorHintPanel hintPanel;
    private SingleNodeConstraintPanel patternPanel;
    
    KahinaControlPointViewPanel viewPanel;
    
    public StepPatternEditorPanel(KahinaControlPointViewPanel viewPanel)
    {
        this.viewPanel = viewPanel;
        
        this.setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
        
        KahinaStepProperty pattern =  viewPanel.view.getModel().getSensor().getStepProperty();
        
        if (pattern instanceof KahinaSimpleProperty)
        {     
            JPanel opsAndHintPanel = new JPanel();
            opsAndHintPanel.setLayout(new BoxLayout(opsAndHintPanel, BoxLayout.LINE_AXIS));
            opsAndHintPanel.setBackground(Color.BLUE);
            
            JPanel boolOpPanel = new JPanel();
            boolOpPanel.setLayout(new BoxLayout(boolOpPanel, BoxLayout.LINE_AXIS));
            boolOpPanel.setBorder(BorderFactory.createTitledBorder("Boolean Ops"));
            
            andOperationButton = new JButton("and");
            andOperationButton.setActionCommand("andOperation");
            andOperationButton.setMargin(new Insets(0, 1, 0, 1));
            andOperationButton.addActionListener(this);
            boolOpPanel.add(andOperationButton);
            
            orOperationButton = new JButton("or");
            orOperationButton.setActionCommand("orOperation");
            orOperationButton.setMargin(new Insets(0, 1, 0, 1));
            orOperationButton.addActionListener(this);
            boolOpPanel.add(orOperationButton);
            
            negOperationButton = new JButton("not");
            negOperationButton.setActionCommand("negOperation");
            negOperationButton.setMargin(new Insets(0, 1, 0, 1));
            negOperationButton.addActionListener(this);
            boolOpPanel.add(negOperationButton);
            
            implOperationButton = new JButton("->");
            implOperationButton.setActionCommand("implOperation");
            implOperationButton.setMargin(new Insets(0, 1, 0, 1));
            implOperationButton.addActionListener(this);
            boolOpPanel.add(implOperationButton);
            
            opsAndHintPanel.add(boolOpPanel);
            
            hintPanel = new BreakpointEditorHintPanel();
            opsAndHintPanel.add(hintPanel);
            
            this.add(opsAndHintPanel);
            
            this.add(Box.createRigidArea(new Dimension(0,0)));
            
            NodeConstraintOptions constrOptions = new NodeConstraintOptions();
            constrOptions.setStandardOptions();

            KahinaSimpleProperty property = (KahinaSimpleProperty) pattern;
            patternPanel = new SingleNodeConstraintPanel(constrOptions, viewPanel.kahina.getControl(), property);
            patternPanel.setHintPanel(hintPanel);
            this.add(patternPanel);
        }
        else if (pattern instanceof KahinaCodeLineProperty)
        {
            KahinaSourceCodeLocation location = ((KahinaCodeLineProperty) pattern).getCodeLocation();
            this.setBorder(new TitledBorder("Source code location"));
            this.add(new JLabel(location.getAbsolutePath() + ":" + location.getLineNumber()));
        }    
        this.add(Box.createVerticalGlue());
    }
    
    @Override
    public void setEnabled(boolean enabled)
    {
        super.setEnabled(enabled);
        setBoolOpsEnabled(enabled);
    }
    
    public void setBoolOpsEnabled(boolean enabled)
    {
        if (enabled)
        {
            activateAllBoolOps();
        }
        else
        {
            deactivateAllBoolOps();
        }
    }
    
    public void activateAllBoolOps()
    {
        andOperationButton.setEnabled(true);
        orOperationButton.setEnabled(true);
        negOperationButton.setEnabled(true);
        implOperationButton.setEnabled(true);
    }
    
    public void deactivateAllBoolOps()
    {
        andOperationButton.setEnabled(false);
        orOperationButton.setEnabled(false);
        negOperationButton.setEnabled(false);
        implOperationButton.setEnabled(false);
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("negOperation"))
        {
            if (patternPanel.getMarkedPattern() != null)
            {
                patternPanel.introduceNegation(patternPanel.getMarkedPattern());
            }
            else
            {
                hint("Select first the constraint to be negated.", Color.RED);
            }
        }
        else if (s.equals("andOperation"))
        {
            if (patternPanel.getMarkedPattern() != null)
            {
                patternPanel.setNodeSelectionMode(KahinaBreakpointEditorPanel.PENDING_AND_OPERATION);
                hint("Now select the second conjunct.", Color.BLACK);
            }
            else
            {
                hint("First conjunct must be selected before clicking this button.", Color.RED);
            }
        }
        else if (s.equals("orOperation"))
        {
            if (patternPanel.getMarkedPattern() != null)
            {
                patternPanel.setNodeSelectionMode(KahinaBreakpointEditorPanel.PENDING_OR_OPERATION);
                hint("Now select the second disjunct.", Color.BLACK);
            }
            else
            {
                hint("First disjunct must be selected before clicking this button.", Color.RED);
            }
        }
        else if (s.equals("implOperation"))
        {
            if (patternPanel.getMarkedPattern() != null)
            {
                patternPanel.setNodeSelectionMode(KahinaBreakpointEditorPanel.PENDING_IMPL_OPERATION);
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
}
