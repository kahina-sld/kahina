package org.kahina.core.visual.breakpoint;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import javax.swing.JColorChooser;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import org.kahina.core.data.breakpoint.KahinaControlPoint;
import org.kahina.core.edit.breakpoint.BreakpointEditorEvent;

public class KahinaControlPointListener implements ActionListener, KeyListener
{
    KahinaControlPointViewPanel viewPanel;
    
    public KahinaControlPointListener()
    {
        this.viewPanel = null;
    }
    
    public void setViewPanel(KahinaControlPointViewPanel viewPanel)
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
            KahinaControlPoint point = viewPanel.view.getModel();
            if (point.isActive())
            {
                point.deactivate();
            }
            else
            {
                point.activate();
            }
            viewPanel.adaptActivationButtonLabel();
            //control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.ACTIVATE_BREAKPOINT, curID));
        } 
        else if (s.equals("exportControlPoint"))
        {
            try
            {
                JFileChooser chooser = new JFileChooser(new File("."));
                chooser.setDialogTitle("Export control point");
                chooser.showSaveDialog(viewPanel);
                File dataFile = chooser.getSelectedFile();
                if (dataFile != null)
                {
                    FileWriter fw = new FileWriter(dataFile);
                    fw.write(viewPanel.view.getModel().exportXML(true));
                    fw.close();
                }
            } 
            catch (IOException ex)
            {
                System.err.println("IOException: Could not export control point!");
                ex.printStackTrace();
            }
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
