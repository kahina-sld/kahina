package org.kahina.gui.breakpoint;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.control.KahinaController;
import org.kahina.control.event.KahinaStateEvent;
import org.kahina.control.event.KahinaSystemEvent;

public class BreakpointEditorFileMenu extends JMenu implements ActionListener
{
    KahinaController control;
    
    public BreakpointEditorFileMenu(KahinaController control)
    {
        super("Breakpoint");
        this.control = control;
        
        JMenuItem newBreakpointItem = new JMenuItem("New...");
        newBreakpointItem.setActionCommand("newBreakpoint");
        newBreakpointItem.addActionListener(this);
        this.add(newBreakpointItem);
        
        this.addSeparator();
            
        JMenuItem exportBreakpointItem = new JMenuItem("Export...");
        exportBreakpointItem.setActionCommand("exportBreakpoint");
        exportBreakpointItem.addActionListener(this);
        this.add(exportBreakpointItem);
        
        JMenuItem importBreakpointItem = new JMenuItem("Import...");
        importBreakpointItem.setActionCommand("importBreakpoint");
        importBreakpointItem.addActionListener(this);
        this.add(importBreakpointItem);
        
        this.addSeparator();
        
        JMenuItem quitItem = new JMenuItem("Quit");
        quitItem.setActionCommand("quit");
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
        else if (s.equals("quit"))
        {
            control.processEvent(new KahinaSystemEvent(KahinaSystemEvent.QUIT));
        }
    }
}
