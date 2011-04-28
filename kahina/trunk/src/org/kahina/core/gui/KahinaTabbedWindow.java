package org.kahina.core.gui;

import javax.swing.JTabbedPane;

import org.kahina.core.control.KahinaController;

public class KahinaTabbedWindow  extends KahinaWindow
{
    JTabbedPane tabbedPane;
    
    public KahinaTabbedWindow(KahinaController control)
    {
    	super(control);
        tabbedPane = new JTabbedPane();
        getContentPane().add(tabbedPane);
    }
    
    public void addWindow(KahinaWindow w)
    {
        tabbedPane.add(w.getTitle(), w.getContentPane());
    }
}
