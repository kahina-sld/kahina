package org.kahina.core.gui.windows;

import java.awt.Container;
import java.awt.dnd.DropTarget;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JTabbedPane;

import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.gui.KahinaWindowTransferHandler;

public class KahinaTabbedWindow  extends KahinaWindow
{
    JTabbedPane tabbedPane;
    
    List<KahinaWindow> windows;
    
    public KahinaTabbedWindow(KahinaWindowManager wm)
    {
    	super(wm);
    	this.initialize();
    }
    
    public KahinaTabbedWindow(KahinaWindowManager wm, int winID)
    {
    	super(wm, winID);
    	this.initialize();
    }
    
    private void initialize()
    {
    	windows = new ArrayList<KahinaWindow>();
    	mainPanel.setTransferHandler(new KahinaWindowTransferHandler());
        mainPanel.setDropTarget(new DropTarget(mainPanel, new KahinaDropTargetListener(this)));
    	
        tabbedPane = new JTabbedPane();
        mainPanel.add(tabbedPane);
    }
    
    public boolean addSubwindow(KahinaWindow w)
    {
    	wm.getArrangement().setEmbeddingWindowID(w.getID(),windowID);
    	windows.add(w);
        tabbedPane.add(w.getTitle(), w.getContentPane());
        return true;
    }
    
    public void addWindow(int index, KahinaWindow w)
    {
    	wm.getArrangement().setEmbeddingWindowID(w.getID(),windowID);
    	windows.add(index, w);
        tabbedPane.add(w.getTitle(), w.getContentPane());
    }
    
	public int getWindowType()
	{
		return KahinaWindowType.TABBED_WINDOW;
	}
    
    public KahinaWindow getReplacementAfterRelease(KahinaWindow removedWindow)
    {
    	int index = windows.indexOf(removedWindow);
    	if (index != -1)
    	{
    		wm.getArrangement().setEmbeddingWindowID(removedWindow.getID(),-1);

    		//crudely determine not too surprising positions and sizes for the separate windows
    		removedWindow.setSize(tabbedPane.getComponents()[index].getSize());
    		removedWindow.setLocation(this.getX() + 30, this.getY() + index * 50);
    		
    		removedWindow.setContentPane((Container) tabbedPane.getComponents()[index]);
    		windows.remove(removedWindow);
    	}
    	else
    	{
    		System.err.println("WARNING: Window \"" + removedWindow.getTitle() + "\" not found as a tab in window \"" + this.getTitle() + "\", release failed.");
    	}
		return this;
    }
    
    public void replaceSubwindow(KahinaWindow oldSubwindow, KahinaWindow newSubwindow)
    {
    	int index = windows.indexOf(oldSubwindow);
    	if (index != -1)
    	{
    		wm.getArrangement().setEmbeddingWindowID(oldSubwindow.getID(),-1);
    		oldSubwindow.setContentPane((Container) tabbedPane.getComponents()[index]);
    		
        	addWindow(index,newSubwindow);
    	}
    	else
    	{
    		System.err.println("WARNING: Window \"" + oldSubwindow.getTitle() + "\" not found as a tab in window \"" + this.getTitle() + "\", replacement failed.");
    	}
    }
}
