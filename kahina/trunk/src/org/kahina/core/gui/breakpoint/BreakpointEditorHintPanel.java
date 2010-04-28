package org.kahina.core.gui.breakpoint;

import java.awt.Color;
import java.awt.Dimension;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;

public class BreakpointEditorHintPanel extends JPanel
{
	private static final long serialVersionUID = -8634880000420210523L;
	
	JLabel hintLabel;
    
    public BreakpointEditorHintPanel()
    {
        setBorder(BorderFactory.createTitledBorder("Hint"));  
        setMaximumSize(new Dimension(2000, 60));
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
