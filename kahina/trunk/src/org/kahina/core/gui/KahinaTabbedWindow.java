package org.kahina.core.gui;

import java.awt.dnd.DropTarget;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JTabbedPane;

public class KahinaTabbedWindow  extends KahinaWindow
{
    JTabbedPane tabbedPane;
    
    List<KahinaWindow> windows;
    
    public KahinaTabbedWindow(KahinaWindowManager wm)
    {
    	super(wm);
    	windows = new ArrayList<KahinaWindow>();
    	mainPanel.setTransferHandler(new KahinaWindowTransferHandler());
        mainPanel.setDropTarget(new DropTarget(mainPanel, new KahinaDropTargetListener(this)));
    	
        tabbedPane = new JTabbedPane();
        mainPanel.add(tabbedPane);
    }
    
    public void addWindow(KahinaWindow w)
    {
    	w.embeddingWindow = this;
    	windows.add(w);
        tabbedPane.add(w.getTitle(), w.getContentPane());
        //tabbedPane.addMouseListener(new KahinaWindowListener(w));
    }
}
