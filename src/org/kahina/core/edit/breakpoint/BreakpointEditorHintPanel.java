package org.kahina.core.edit.breakpoint;

import java.awt.Color;
import java.awt.Dimension;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;

public class BreakpointEditorHintPanel extends JPanel
{
    JLabel hintLabel;
    
    public BreakpointEditorHintPanel()
    {
        this.setLayout(new BoxLayout(this, BoxLayout.LINE_AXIS));
        this.setBorder(BorderFactory.createTitledBorder("Hint")); 
        setMaximumSize(new Dimension(2000, 60));
        
        hintLabel = new JLabel("Define a node constraint by selecting a type.");
        this.add(hintLabel);
        //hintLabel.getBorder().getBorderInsets(this).bottom = 0;
        //hintLabel.getBorder().getBorderInsets(this).top = 0;
        
        this.add(Box.createHorizontalGlue());
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
    
    @Override
	public void setEnabled(boolean enabled)
    {
        super.setEnabled(enabled);
        hintLabel.setEnabled(enabled);
    }
}
