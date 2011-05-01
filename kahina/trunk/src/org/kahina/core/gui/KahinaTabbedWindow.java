package org.kahina.core.gui;

import java.util.ArrayList;
import java.util.List;

import javax.swing.JTabbedPane;

import org.kahina.core.control.KahinaController;

public class KahinaTabbedWindow  extends KahinaWindow
{
    JTabbedPane tabbedPane;
    
    List<KahinaWindow> windows;
    
    public KahinaTabbedWindow(KahinaWindowManager wm)
    {
    	super(wm);
    	windows = new ArrayList<KahinaWindow>();
    	
        tabbedPane = new JTabbedPane();
        getContentPane().add(tabbedPane);
    }
    
    public void addWindow(KahinaWindow w)
    {
    	w.embeddingWindow = this;
    	windows.add(w);
        tabbedPane.add(w.getTitle(), w.getContentPane()).addMouseListener(new KahinaWindowListener(w));
    }
}
