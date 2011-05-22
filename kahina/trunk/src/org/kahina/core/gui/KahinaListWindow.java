package org.kahina.core.gui;

import java.awt.Container;
import java.awt.dnd.DropTarget;
import java.util.ArrayList;
import java.util.List;

import javax.swing.DefaultListModel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

public class KahinaListWindow extends KahinaWindow implements ListSelectionListener
{
	DefaultListModel listModel;
	JList list;
	JPanel displayPane;
    
    List<KahinaWindow> windows;
    int selectedWindow;
    
    public KahinaListWindow(KahinaWindowManager wm)
    {
    	super(wm);
    	this.initialize();
    }
    
    public KahinaListWindow(KahinaWindowManager wm, int winID)
    {
    	super(wm, winID);
    	this.initialize();
    }
    
    private void initialize()
    {
    	windows = new ArrayList<KahinaWindow>();
    	selectedWindow = -1;

    	listModel = new DefaultListModel();
        list = new JList(listModel);
    	list.setTransferHandler(new KahinaWindowTransferHandler());
        list.setDropTarget(new DropTarget(mainPanel, new KahinaDropTargetListener(this)));
        list.addListSelectionListener(this);
        mainPanel.add(list);
        
        displayPane = new JPanel();
        mainPanel.add(displayPane);
    }
    
    public boolean addSubwindow(KahinaWindow w)
    {
    	wm.arr.setEmbeddingWindowID(w.getID(),windowID);
    	windows.add(w);
        listModel.addElement(w);
        selectedWindow = windows.size() - 1;
        return true;
    }
    
    public void addWindow(int index, KahinaWindow w)
    {
    	wm.arr.setEmbeddingWindowID(w.getID(),windowID);
    	windows.add(index, w);
        listModel.insertElementAt(w,index);
    }
    
	public int getWindowType()
	{
		return KahinaWindowType.LIST_WINDOW;
	}
    
    public KahinaWindow getReplacementAfterRelease(KahinaWindow removedWindow)
    {
    	int index = windows.indexOf(removedWindow);
    	if (index != -1)
    	{
    		wm.arr.setEmbeddingWindowID(removedWindow.getID(),-1);
    		
    		if (index == selectedWindow)
    		{
    	  		removedWindow.setContentPane((Container) displayPane.getComponents()[0]);
        		list.setSelectedIndex(--selectedWindow);
    		}
    		//crudely determine not too surprising positions and sizes for the separate windows
    		removedWindow.setSize(removedWindow.getComponents()[0].getSize());
    		removedWindow.setLocation(this.getX() + 30, this.getY() + index * 50);
    		
    		windows.remove(removedWindow);
    		listModel.remove(index);
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
    	if (index == selectedWindow)
    	{
    		wm.arr.setEmbeddingWindowID(oldSubwindow.getID(),-1);
    		oldSubwindow.setContentPane((Container) displayPane.getComponents()[0]);
    		list.setSelectedIndex(--selectedWindow);
        	addWindow(index,newSubwindow);
    	}
    	else if (index != -1)
    	{
    		wm.arr.setEmbeddingWindowID(oldSubwindow.getID(),-1);
        	addWindow(index,newSubwindow);
    	}
    	else
    	{
    		System.err.println("WARNING: Window \"" + oldSubwindow.getTitle() + "\" not found as a tab in window \"" + this.getTitle() + "\", replacement failed.");
    	}
    }

	@Override
	public void valueChanged(ListSelectionEvent arg0) 
	{
		int oldSelectedWindow = selectedWindow;
		selectedWindow = arg0.getFirstIndex();
		if (oldSelectedWindow != -1)
		{
			windows.get(oldSelectedWindow).setContentPane((Container) displayPane.getComponents()[0]);
		}
		if (selectedWindow != -1)
		{
			displayPane.add(windows.get(selectedWindow).getContentPane());
		}	
	}
}
