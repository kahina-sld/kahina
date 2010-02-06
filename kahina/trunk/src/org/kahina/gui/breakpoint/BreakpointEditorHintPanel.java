package org.kahina.gui.breakpoint;

import java.awt.Color;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;

public class BreakpointEditorHintPanel extends JPanel
{
    JLabel hintLabel;
    
    public BreakpointEditorHintPanel()
    {
        setBorder(BorderFactory.createTitledBorder("Hint"));  
        hintLabel = new JLabel("Define a node constraint by selecting a type.");
        add(hintLabel);
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
    
    public void setEnabled(boolean enabled)
    {
        super.setEnabled(enabled);
        hintLabel.setEnabled(enabled);
    }
}
