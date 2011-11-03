package org.kahina.core.gui;

import java.awt.Container;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.border.TitledBorder;

import org.kahina.core.control.KahinaController;

public class KahinaVerticallySplitWindow extends KahinaWindow
{
	private static final long serialVersionUID = -4836711113431179305L;
	
	KahinaWindow upperWindow;
    KahinaWindow lowerWindow;
    
    JPanel topPanel;
    JPanel bottomPanel;
    
    JSplitPane splitPane;
    
    private double resizeWeight;
    
    public KahinaVerticallySplitWindow(KahinaWindowManager wm, double resizeWeight)
    {
    	super(wm);
    	this.resizeWeight = resizeWeight;
    	this.initialize();
    }
    
    public KahinaVerticallySplitWindow(KahinaWindowManager wm, int winID, double resizeWeight)
    {
    	super(wm,winID);
    	this.resizeWeight = resizeWeight;
    	this.initialize();
    }
    
    private void initialize()
    {
        topPanel = new JPanel();
        topPanel.setLayout(new BoxLayout(topPanel, BoxLayout.Y_AXIS));
        bottomPanel = new JPanel();
        bottomPanel.setLayout(new BoxLayout(bottomPanel, BoxLayout.Y_AXIS));
        splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, topPanel, bottomPanel);
        splitPane.setDividerSize(2);
        splitPane.setResizeWeight(resizeWeight);
        splitPane.addPropertyChangeListener(JSplitPane.DIVIDER_LOCATION_PROPERTY, 
        	new PropertyChangeListener() 
        	{
	        	public void propertyChange(PropertyChangeEvent pce) 
        		{
	        		//System.err.println("changedVerticalDividerLocation(" + windowID + ")");
	        		if (upperWindow != null)
	        		{
	        			wm.arr.setSize(upperWindow.windowID, getWidth(), splitPane.getDividerLocation());
	        		}
	        		if (lowerWindow != null)
	        		{
	        			wm.arr.setSize(lowerWindow.windowID, getWidth(), getHeight() - splitPane.getDividerLocation());
	        		}
        		}
        	}
        );
        mainPanel.add(splitPane);
    }
    
    public boolean addSubwindow(KahinaWindow w)
    {
    	if (upperWindow == null)
    	{
    		setUpperWindow(w);
    		adaptDividerLocation();
    		return true;
    	}
    	else if (lowerWindow == null)
    	{
    		setLowerWindow(w);
    		adaptDividerLocation();
    		return true;
    	}
    	else
    	{
    		return false;
    	}
    }
    
    public void setUpperWindow(KahinaWindow w)
    {
    	wm.arr.setEmbeddingWindowID(w.getID(),windowID);
    	upperWindow = w;
        topPanel.removeAll();
        topPanel.add(w.getContentPane());
    }
    
    public void setLowerWindow(KahinaWindow w)
    {
    	wm.arr.setEmbeddingWindowID(w.getID(),windowID);
    	lowerWindow = w;
        bottomPanel.removeAll();
        bottomPanel.add(w.getContentPane());
    }
    
    private void adaptDividerLocation()
    {
    	int upperHeight = 1;
    	if (upperWindow != null) upperHeight = wm.arr.getHeight(upperWindow.getID());
    	//System.err.println("adaptVerticalDividerLocation(" + windowID + "," + upperHeight + ")");
    	splitPane.setDividerLocation(upperHeight);
        splitPane.setResizeWeight(resizeWeight);
    }
    
    public KahinaWindow getReplacementAfterRelease(KahinaWindow removedWindow)
    {
    	if (upperWindow == removedWindow)
    	{
    		wm.arr.setEmbeddingWindowID(upperWindow.getID(),-1);
    		upperWindow.setContentPane((Container) topPanel.getComponents()[0]);		
    		wm.arr.setEmbeddingWindowID(lowerWindow.getID(),-1);
    		lowerWindow.setContentPane((Container) bottomPanel.getComponents()[0]);
    		//crudely determine not too surprising positions and sizes for the separate windows
    		upperWindow.setSize(topPanel.getSize());
    		lowerWindow.setSize(bottomPanel.getSize());
    		upperWindow.setLocation(this.getLocation());
    		lowerWindow.setLocation(this.getX(), this.getY() + topPanel.getHeight());
    		return lowerWindow;
    	}
    	else if (lowerWindow == removedWindow)
    	{
    		wm.arr.setEmbeddingWindowID(upperWindow.getID(),-1);
    		upperWindow.setContentPane((Container) topPanel.getComponents()[0]);		
    		wm.arr.setEmbeddingWindowID(lowerWindow.getID(),-1);
    		lowerWindow.setContentPane((Container) bottomPanel.getComponents()[0]);
    		//crudely determine not too surprising positions and sizes for the separate windows
    		upperWindow.setSize(topPanel.getSize());
    		lowerWindow.setSize(bottomPanel.getSize());
    		lowerWindow.setLocation(this.getX(), this.getY() + topPanel.getHeight());
    		upperWindow.setLocation(this.getLocation());
    		return upperWindow;
    	}
    	else
    	{
    		System.err.println("WARNING: Window \"" + removedWindow.getTitle() + "\" is not a subwindow of window \"" + this.getTitle() + "\", release failed.");
    		return this;
    	}
    }
    
    public void replaceSubwindow(KahinaWindow oldSubwindow, KahinaWindow newSubwindow)
    {
       	if (upperWindow == oldSubwindow)
    	{
    		wm.arr.setEmbeddingWindowID(oldSubwindow.getID(),-1);
       		setUpperWindow(newSubwindow);
    	}
    	else if (lowerWindow == oldSubwindow)
    	{
    		wm.arr.setEmbeddingWindowID(oldSubwindow.getID(),-1);
       		setLowerWindow(newSubwindow);
    	}
    	else
    	{
    		System.err.println("WARNING: Window \"" + oldSubwindow.getTitle() + "\" is not a subwindow of window \"" + this.getTitle() + "\", replacement failed.");
    	}
       	adaptDividerLocation();
    }
    
	public int getWindowType()
	{
		return KahinaWindowType.VERT_SPLIT_WINDOW;
	}
    
    public boolean isFlippableWindow()
    {
    	return true;
    }
    
    public void flipSubwindows()
    {
    	//System.err.println("flipSubwindows(" + windowID + ")");
    	KahinaWindow tempWindow = upperWindow;
    	setUpperWindow(lowerWindow);
    	setLowerWindow(tempWindow);
    	adaptDividerLocation();
    }
    
    public void flipSubwindowsIfIndicatedByCoordinates()
    {
    	if (wm.arr.getYPos(upperWindow.getID()) > wm.arr.getYPos(lowerWindow.getID()))
    	{
    		flipSubwindows();
    	}
    }
    
    public KahinaWindow createDynamicClone()
    {
    	KahinaVerticallySplitWindow cloneWindow = new KahinaVerticallySplitWindow(wm, resizeWeight);
    	cloneWindow.cloned = true;
    	cloneWindow.setTitle(getTitle() + " (clone)");
    	cloneWindow.setUpperWindow(upperWindow.createDynamicClone());
    	cloneWindow.setLowerWindow(lowerWindow.createDynamicClone());
    	cloneWindow.setSize(this.getSize());
    	cloneWindow.setLocation(this.getX() + 100, this.getY() + 100);
    	return cloneWindow;
    }
    
    public KahinaWindow createSnapshotClone()
    {
    	KahinaVerticallySplitWindow cloneWindow = new KahinaVerticallySplitWindow(wm, resizeWeight);
    	cloneWindow.cloned = true;
    	cloneWindow.setTitle(getTitle() + " (at step " + wm.gui.kahina.getState().nextStepID() + ")");
    	cloneWindow.setUpperWindow(upperWindow.createSnapshotClone());
    	cloneWindow.setLowerWindow(lowerWindow.createSnapshotClone());
    	cloneWindow.setSize(this.getSize());
    	cloneWindow.setLocation(this.getX() + 100, this.getY() + 100);
    	return cloneWindow;
    }
}
