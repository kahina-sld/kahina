package org.kahina.core.visual.breakpoint;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JColorChooser;
import javax.swing.JOptionPane;

import org.kahina.core.edit.breakpoint.BreakpointEditorEvent;

public class KahinaControlPointListener implements ActionListener
{
    KahinaControlPointViewPanel viewPanel;
    
    public KahinaControlPointListener(KahinaControlPointViewPanel viewPanel)
    {
        this.viewPanel = viewPanel;
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("changeColor"))
        {
            Color newColor = JColorChooser.showDialog(viewPanel,"Choose Background Color",viewPanel.colorButton.getBackground());
            viewPanel.colorButton.setBackground(newColor);
            //TODO: save current breakpoint for adaptation in view
            //breakpoint.setSignalColor(newColor);
        }
        else if (s.equals("suggestName"))
        {
            //breakpoint.setName(breakpoint.getPattern().toString());
            //viewPanel.nameEditLine.setText(breakpoint.getName());
            //control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.BREAKPOINT_NAME_UPDATE));
        }
    }
}
