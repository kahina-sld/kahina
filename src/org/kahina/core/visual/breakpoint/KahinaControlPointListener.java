package org.kahina.core.visual.breakpoint;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.JColorChooser;
import javax.swing.JOptionPane;

import org.kahina.core.data.breakpoint.KahinaControlPoint;
import org.kahina.core.edit.breakpoint.BreakpointEditorEvent;

public class KahinaControlPointListener implements ActionListener, KeyListener
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
            viewPanel.view.getModel().setSignalColor(newColor);
        }
        else if (s.equals("suggestName"))
        {
            KahinaControlPoint point = viewPanel.view.getModel();
            point.setName(point.getPattern().toString());
            viewPanel.processNameChange();
        }
        else if (s.equals("toggleActivation"))
        {
            System.err.println("toggleActivation");
            KahinaControlPoint point = viewPanel.view.getModel();
            if (point.isActive())
            {
                point.deactivate();
            }
            else
            {
                point.activate();
            }
            //TODO: find out how to safely update this
            viewPanel.adaptActivationButtonLabel();
            //control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.ACTIVATE_BREAKPOINT, curID));
        } 
    }
    
    public void keyPressed(KeyEvent e) 
    {
    }

    public void keyReleased(KeyEvent e) 
    {
        String val = viewPanel.nameEditLine.getText();
        viewPanel.view.getModel().setName(val);
        viewPanel.processNameChange();
    }

    public void keyTyped(KeyEvent e) 
    {

    }
}
