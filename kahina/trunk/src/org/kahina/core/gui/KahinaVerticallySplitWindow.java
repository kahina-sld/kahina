package org.kahina.core.gui;

import java.awt.Container;

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
    
    public KahinaVerticallySplitWindow(KahinaWindowManager wm)
    {
    	super(wm);
        topPanel = new JPanel();
        topPanel.setLayout(new BoxLayout(topPanel, BoxLayout.Y_AXIS));
        bottomPanel = new JPanel();
        bottomPanel.setLayout(new BoxLayout(bottomPanel, BoxLayout.Y_AXIS));
        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, topPanel, bottomPanel);
        splitPane.setResizeWeight(.5);
        mainPanel.add(splitPane);
    }
    
    public void setUpperWindow(KahinaWindow w)
    {
    	w.embeddingWindow = this;
    	upperWindow = w;
        topPanel.removeAll();
        topPanel.add(w.getContentPane());
    }
    
    public void setLowerWindow(KahinaWindow w)
    {
    	w.embeddingWindow = this;
    	lowerWindow = w;
        bottomPanel.removeAll();
        bottomPanel.add(w.getContentPane());
    }
    
    public KahinaWindow getReplacementAfterRelease(KahinaWindow removedWindow)
    {
    	if (upperWindow == removedWindow)
    	{
    		upperWindow.embeddingWindow = null;
    		upperWindow.setContentPane((Container) topPanel.getComponents()[0]);		
    		lowerWindow.embeddingWindow = null;
    		lowerWindow.setContentPane((Container) bottomPanel.getComponents()[0]);
			//determine too surprising positions and sizes for the separate windows
    		upperWindow.setSize(topPanel.getSize());
    		lowerWindow.setSize(bottomPanel.getSize());
    		upperWindow.setLocation(this.getLocation());
    		lowerWindow.setLocation(this.getX(), this.getY() + topPanel.getHeight());
    		return lowerWindow;
    	}
    	else if (lowerWindow == removedWindow)
    	{
    		upperWindow.embeddingWindow = null;
    		upperWindow.setContentPane((Container) topPanel.getComponents()[0]);		
    		lowerWindow.embeddingWindow = null;
    		lowerWindow.setContentPane((Container) bottomPanel.getComponents()[0]);
			//determine too surprising positions and sizes for the separate windows
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
       		oldSubwindow.embeddingWindow = null;
       		setUpperWindow(newSubwindow);
    	}
    	else if (lowerWindow == oldSubwindow)
    	{
       		oldSubwindow.embeddingWindow = null;
       		setLowerWindow(newSubwindow);
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
    	KahinaWindow tempWindow = upperWindow;
    	setUpperWindow(lowerWindow);
    	setLowerWindow(tempWindow);
    }
}
