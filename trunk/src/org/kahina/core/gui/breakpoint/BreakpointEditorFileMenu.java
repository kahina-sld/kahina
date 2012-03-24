package org.kahina.core.gui.breakpoint;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.core.control.KahinaController;
import org.kahina.core.edit.breakpoint.BreakpointEditorEvent;

public class BreakpointEditorFileMenu extends JMenu implements ActionListener
{
 
	private static final long serialVersionUID = 7163623733563539175L;
	KahinaController control;
    
    public BreakpointEditorFileMenu(KahinaController control)
    {
        super("Profile");
        this.control = control;
        
        JMenuItem newBreakpointProfileItem = new JMenuItem("New Profile");
        newBreakpointProfileItem.setActionCommand("newBreakpointProfile");
        newBreakpointProfileItem.addActionListener(this);
        //newBreakpointProfileItem.setEnabled(false);
        this.add(newBreakpointProfileItem);
        
        this.addSeparator();
        
        JMenuItem exportBreakpointProfileItem = new JMenuItem("Export Profile");
        exportBreakpointProfileItem.setActionCommand("exportBreakpointProfile");
        exportBreakpointProfileItem.addActionListener(this);
        //exportBreakpointProfileItem.setEnabled(false);
        this.add(exportBreakpointProfileItem);
        
        JMenuItem importBreakpointProfileItem = new JMenuItem("Import Profile");
        importBreakpointProfileItem.setActionCommand("importBreakpointProfile");
        importBreakpointProfileItem.addActionListener(this);
        //importBreakpointItem.setEnabled(false);
        this.add(importBreakpointProfileItem);
        
        this.addSeparator();
            
        JMenuItem exportBreakpointItem = new JMenuItem("Export Breakpoint");
        exportBreakpointItem.setActionCommand("exportBreakpoint");
        exportBreakpointItem.addActionListener(this);
        //exportBreakpointItem.setEnabled(false);
        this.add(exportBreakpointItem);
        
        JMenuItem importBreakpointItem = new JMenuItem("Import Breakpoint");
        importBreakpointItem.setActionCommand("importBreakpoint");
        importBreakpointItem.addActionListener(this);
        //importBreakpointItem.setEnabled(false);
        this.add(importBreakpointItem);
        
        this.addSeparator();
        
        JMenuItem testBreakpointsItem = new JMenuItem("Test");
        testBreakpointsItem.setActionCommand("testBreakpoints");
        testBreakpointsItem.addActionListener(this);
        this.add(testBreakpointsItem);
        
        this.addSeparator();
        
        JMenuItem quitItem = new JMenuItem("Apply and Quit");
        quitItem.setActionCommand("applyQuit");
        quitItem.addActionListener(this);
        this.add(quitItem);
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("newBreakpoint"))
        {
            control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.NEW_BREAKPOINT));
        }
        else if (s.equals("exportBreakpointProfile"))
        {
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Export breakpoint profile");
            chooser.showSaveDialog(this);
            File dataFile = chooser.getSelectedFile();
            if (dataFile != null) control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.EXPORT_BREAKPOINT_PROFILE, dataFile));
        }
        else if (s.equals("importBreakpointProfile"))
        {
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Import breakpoint profile");
            chooser.showOpenDialog(this);
            File dataFile = chooser.getSelectedFile();
            if (dataFile != null) control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.IMPORT_BREAKPOINT_PROFILE, dataFile));
        }
        else if (s.equals("exportBreakpoint"))
        {
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Export breakpoint");
            chooser.showSaveDialog(this);
            File dataFile = chooser.getSelectedFile();
            if (dataFile != null) control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.EXPORT_BREAKPOINT, dataFile));
        }
        else if (s.equals("importBreakpoint"))
        {
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Import breakpoint");
            chooser.showOpenDialog(this);
            File dataFile = chooser.getSelectedFile();
            if (dataFile != null) control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.IMPORT_BREAKPOINT, dataFile));
        }
        else if (s.equals("testBreakpoints"))
        {
            control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.TEST_BREAKPOINTS));
        }
        else if (s.equals("applyQuit"))
        {
            control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.APPLY_QUIT));
        }
    }
}
