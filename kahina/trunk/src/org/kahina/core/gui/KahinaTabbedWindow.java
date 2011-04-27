package org.kahina.core.gui;

import javax.swing.JTabbedPane;

public class KahinaTabbedWindow  extends KahinaWindow
{
    JTabbedPane tabbedPane;
    
    public KahinaTabbedWindow()
    {
        tabbedPane = new JTabbedPane();
        getContentPane().add(tabbedPane);
    }
    
    public void addWindow(KahinaWindow w)
    {
        tabbedPane.add(w.getTitle(), w.getContentPane());
    }
}
