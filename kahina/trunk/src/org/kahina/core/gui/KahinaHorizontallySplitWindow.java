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
    
    public KahinaHorizontallySplitWindow(KahinaWindowManager wm)
    {
    	super(wm);
        leftPanel = new JPanel();
        leftPanel.setLayout(new BoxLayout(leftPanel, BoxLayout.Y_AXIS));
        rightPanel = new JPanel();
        rightPanel.setLayout(new BoxLayout(rightPanel, BoxLayout.Y_AXIS));
        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, leftPanel, rightPanel);
        splitPane.setResizeWeight(.5);
        mainPanel.add(splitPane);
    }
    
    public void setLeftWindow(KahinaWindow w)
    {
    	w.embeddingWindow = this;
    	leftWindow = w;
        leftPanel.removeAll();
        leftPanel.add(w.getContentPane());
    }
    
    public void setRightWindow(KahinaWindow w)
    {
    	w.embeddingWindow = this;
    	rightWindow = w;
        rightPanel.removeAll();
        rightPanel.add(w.getContentPane());
    }
    
    public KahinaWindow getReplacementAfterRelease(KahinaWindow removedWindow)
    {
    	if (leftWindow == removedWindow)
    	{
    		leftWindow.embeddingWindow = null;
    		leftWindow.setContentPane((Container) leftPanel.getComponents()[0]);		
    		rightWindow.embeddingWindow = null;
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
    		leftWindow.embeddingWindow = null;
    		leftWindow.setContentPane((Container) leftPanel.getComponents()[0]);		
    		rightWindow.embeddingWindow = null;
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
       		oldSubwindow.embeddingWindow = null;
       		setLeftWindow(newSubwindow);
    	}
    	else if (rightWindow == oldSubwindow)
    	{
       		oldSubwindow.embeddingWindow = null;
       		setRightWindow(newSubwindow);
    	}
    	else
    	{
    		System.err.println("WARNING: Window \"" + oldSubwindow.getTitle() + "\" is not a subwindow of window \"" + this.getTitle() + "\", replacement failed.");
    	}
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
