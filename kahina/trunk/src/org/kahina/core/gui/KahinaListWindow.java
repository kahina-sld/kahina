package org.kahina.core.gui;

import java.awt.Container;
import java.awt.dnd.DropTarget;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

public class KahinaListWindow extends KahinaWindow implements MouseListener
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
        list.addMouseListener(this);
        
        displayPane = new JPanel();
        displayPane.setLayout(new BoxLayout(displayPane, BoxLayout.Y_AXIS));
        
        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, list, displayPane);
        splitPane.setResizeWeight(.3);
        mainPanel.add(splitPane);
    }
    
    public boolean addSubwindow(KahinaWindow w)
    {
    	wm.arr.setEmbeddingWindowID(w.getID(),windowID);
    	windows.add(w);
        listModel.addElement(w);
        selectWindow(windows.size() - 1);
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
	
	/**
	 * Selects an index of the list and displays the associated window on the right.
	 * @param index the index to be displayed; -1 for no selection
	 */
	public void selectWindow(int index)
	{
		if (index == -1)
		{
			if (selectedWindow != -1)
			{
				windows.get(selectedWindow).setContentPane((Container) displayPane.getComponents()[0]);
			}
		}
		else if (index < windows.size())
		{
			int oldSelectedWindow = selectedWindow;
			selectedWindow = index;
			System.err.println("oldSelectedWindow: " + oldSelectedWindow);
			if (oldSelectedWindow != -1)
			{
				windows.get(oldSelectedWindow).setContentPane((Container) displayPane.getComponents()[0]);
			}
			displayPane.add(windows.get(selectedWindow).getContentPane());
		}
		else
		{
			System.err.println("WARNING: could not select index " + index + " in list window.");
		}
		displayPane.repaint();
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
    	  		int oldSelectedWindow = selectedWindow;
    	  		selectedWindow = -1;
        		selectWindow(oldSelectedWindow - 1);     		
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
    		selectWindow(--selectedWindow);
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
    
    public void mouseClicked(MouseEvent e) 
    {
    	selectWindow(list.locationToIndex(e.getPoint()));
    }

	@Override
	public void mouseEntered(MouseEvent arg0) 
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mouseExited(MouseEvent arg0) 
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mousePressed(MouseEvent arg0) 
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mouseReleased(MouseEvent arg0) 
	{
		// TODO Auto-generated method stub
		
	}

}
