package org.kahina.core.gui;

import java.awt.Container;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.border.TitledBorder;

import org.kahina.core.control.KahinaController;

public class KahinaHorizontallySplitWindow extends KahinaWindow
{
	private static final long serialVersionUID = 7735328776548607273L;
	
	KahinaWindow leftWindow;
    KahinaWindow rightWindow;
    
    JPanel leftPanel;
    JPanel rightPanel;
    
    JSplitPane splitPane;
    
    public KahinaHorizontallySplitWindow(KahinaWindowManager wm)
    {
    	super(wm);
    	this.initialize();
    }
    
    public KahinaHorizontallySplitWindow(KahinaWindowManager wm, int winID)
    {
    	super(wm,winID);
    	this.initialize();
    }
    
    private void initialize()
    {
        leftPanel = new JPanel();
        leftPanel.setLayout(new BoxLayout(leftPanel, BoxLayout.Y_AXIS));
        rightPanel = new JPanel();
        rightPanel.setLayout(new BoxLayout(rightPanel, BoxLayout.Y_AXIS));
        splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, leftPanel, rightPanel);
        splitPane.setDividerSize(2);
        splitPane.setResizeWeight(.5);
        mainPanel.add(splitPane);
    }
    
    public boolean addSubwindow(KahinaWindow w)
    {
    	if (leftWindow == null)
    	{
    		setLeftWindow(w);
    		return true;
    	}
    	else if (rightWindow == null)
    	{
    		setRightWindow(w);
    		return true;
    	}
    	else
    	{
    		return false;
    	}
    }
    
    public void setLeftWindow(KahinaWindow w)
    {
    	wm.arr.setEmbeddingWindowID(w.getID(),windowID);
    	leftWindow = w;
        leftPanel.removeAll();
        leftPanel.add(w.getContentPane());
        adaptDividerLocation();
    }
    
    public void setRightWindow(KahinaWindow w)
    {
    	wm.arr.setEmbeddingWindowID(w.getID(),windowID);
    	rightWindow = w;
        rightPanel.removeAll();
        rightPanel.add(w.getContentPane());
        adaptDividerLocation();
    }
    
    private void adaptDividerLocation()
    {
    	int leftWidth = 1;
    	if (leftWindow != null) leftWidth = wm.arr.getWidth(leftWindow.getID());
    	splitPane.setDividerLocation(leftWidth);
        splitPane.setResizeWeight(0.5);
    }
    
    public KahinaWindow getReplacementAfterRelease(KahinaWindow removedWindow)
    {
    	if (leftWindow == removedWindow)
    	{
    		wm.arr.setEmbeddingWindowID(leftWindow.getID(),-1);
    		leftWindow.setContentPane((Container) leftPanel.getComponents()[0]);		
    		wm.arr.setEmbeddingWindowID(rightWindow.getID(),-1);
    		rightWindow.setContentPane((Container) rightPanel.getComponents()[0]);
			//crudely determine not too surprising positions and sizes for the separate windows
    		leftWindow.setSize(leftPanel.getSize());
    		rightWindow.setSize(rightPanel.getSize());
    		leftWindow.setLocation(this.getLocation());
    		rightWindow.setLocation(this.getX() + leftPanel.getWidth(), this.getY());
    		return rightWindow;
    	}
    	else if (rightWindow == removedWindow)
    	{
    		wm.arr.setEmbeddingWindowID(leftWindow.getID(),-1);
    		leftWindow.setContentPane((Container) leftPanel.getComponents()[0]);		
    		wm.arr.setEmbeddingWindowID(rightWindow.getID(),-1);
    		rightWindow.setContentPane((Container) rightPanel.getComponents()[0]);
			//crudely determine not too surprising positions and sizes for the separate windows
    		leftWindow.setSize(leftPanel.getSize());
    		rightWindow.setSize(rightPanel.getSize());
    		rightWindow.setLocation(this.getX() + leftPanel.getWidth(), this.getY());
    		leftWindow.setLocation(this.getLocation());
    		return leftWindow;
    	}
    	else
    	{
    		System.err.println("WARNING: Window \"" + removedWindow.getTitle() + "\" is not a subwindow of window \"" + this.getTitle() + "\", release failed.");
    		return this;
    	}
    }
    
    public void replaceSubwindow(KahinaWindow oldSubwindow, KahinaWindow newSubwindow)
    {
       	if (leftWindow == oldSubwindow)
    	{
    		wm.arr.setEmbeddingWindowID(oldSubwindow.getID(),-1);
       		setLeftWindow(newSubwindow);
    	}
    	else if (rightWindow == oldSubwindow)
    	{
    		wm.arr.setEmbeddingWindowID(oldSubwindow.getID(),-1);
       		setRightWindow(newSubwindow);
    	}
    	else
    	{
    		System.err.println("WARNING: Window \"" + oldSubwindow.getTitle() + "\" is not a subwindow of window \"" + this.getTitle() + "\", replacement failed.");
    	}
        adaptDividerLocation();
    }
    
	public int getWindowType()
	{
		return KahinaWindowType.HORI_SPLIT_WINDOW;
	}
    
    public boolean isFlippableWindow()
    {
    	return true;
    }
    
    public void flipSubwindows()
    {
    	KahinaWindow tempWindow = leftWindow;
    	setLeftWindow(rightWindow);
    	setRightWindow(tempWindow);
    }
    
    public void flipSubwindowsIfIndicatedByCoordinates()
    {
    	if (wm.arr.getXPos(leftWindow.getID()) > wm.arr.getXPos(rightWindow.getID()))
    	{
    		flipSubwindows();
    	}
    }
    
    public KahinaWindow createDynamicClone()
    {
    	KahinaHorizontallySplitWindow cloneWindow = new KahinaHorizontallySplitWindow(wm);
    	cloneWindow.cloned = true;
    	cloneWindow.setTitle(getTitle() + " (clone)");
    	cloneWindow.setLeftWindow(leftWindow.createDynamicClone());
    	cloneWindow.setRightWindow(rightWindow.createDynamicClone());
    	cloneWindow.setSize(this.getSize());
    	cloneWindow.setLocation(this.getX() + 100, this.getY() + 100);
    	return cloneWindow;
    }
    
    public KahinaWindow createSnapshotClone()
    {
    	KahinaHorizontallySplitWindow cloneWindow = new KahinaHorizontallySplitWindow(wm);
    	cloneWindow.cloned = true;
    	cloneWindow.setTitle(getTitle() + " (at step " + wm.gui.kahina.getState().nextStepID() + ")");
    	cloneWindow.setLeftWindow(leftWindow.createSnapshotClone());
    	cloneWindow.setRightWindow(rightWindow.createSnapshotClone());
    	cloneWindow.setSize(this.getSize());
    	cloneWindow.setLocation(this.getX() + 100, this.getY() + 100);
    	return cloneWindow;
    }
}
