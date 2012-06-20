package org.kahina.core.visual.breakpoint;

import java.awt.Dimension;
import java.awt.Insets;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;

import org.kahina.core.edit.breakpoint.BreakpointEditorHintPanel;

public class StepPatternEditorPanel extends JPanel
{   
    JButton andOperationButton;
    JButton orOperationButton;
    JButton negOperationButton;
    JButton implOperationButton;
    
    BreakpointEditorHintPanel hintPanel;
    
    public StepPatternEditorPanel()
    {
        this.setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
        
        JPanel opsAndHintPanel = new JPanel();
        opsAndHintPanel.setLayout(new BoxLayout(opsAndHintPanel, BoxLayout.LINE_AXIS));
        
        JPanel boolOpPanel = new JPanel();
        boolOpPanel.setLayout(new BoxLayout(boolOpPanel, BoxLayout.LINE_AXIS));
        boolOpPanel.setBorder(BorderFactory.createTitledBorder("Boolean Ops"));
        
        andOperationButton = new JButton("and");
        andOperationButton.setActionCommand("andOperation");
        andOperationButton.setMargin(new Insets(0, 1, 0, 1));
        //andOperationButton.addActionListener(listener);
        boolOpPanel.add(andOperationButton);
        
        orOperationButton = new JButton("or");
        orOperationButton.setActionCommand("orOperation");
        orOperationButton.setMargin(new Insets(0, 1, 0, 1));
        //orOperationButton.addActionListener(listener);
        boolOpPanel.add(orOperationButton);
        
        negOperationButton = new JButton("not");
        negOperationButton.setActionCommand("negOperation");
        negOperationButton.setMargin(new Insets(0, 1, 0, 1));
        //negOperationButton.addActionListener(listener);
        boolOpPanel.add(negOperationButton);
        
        implOperationButton = new JButton("->");
        implOperationButton.setActionCommand("implOperation");
        implOperationButton.setMargin(new Insets(0, 1, 0, 1));
        //implOperationButton.addActionListener(listener);
        boolOpPanel.add(implOperationButton);
        
        opsAndHintPanel.add(boolOpPanel);
        
        hintPanel = new BreakpointEditorHintPanel();
        opsAndHintPanel.add(hintPanel);
        
        this.add(opsAndHintPanel);
        
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
}
