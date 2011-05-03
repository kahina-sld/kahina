package org.kahina.core.gui;

import java.awt.BorderLayout;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.BorderFactory;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;

import org.kahina.core.control.KahinaController;
import org.kahina.core.event.KahinaWindowEvent;
import org.kahina.core.event.KahinaWindowEventType;

public class KahinaWindow extends JFrame implements WindowListener
{
	private static final long serialVersionUID = 6613805267152521669L; 
    private static final boolean verbose = false; 
    
    private static int idCounter = 0;
    
    KahinaWindowManager wm;
    KahinaTransferablePanel mainPanel;
    
    //link upward in embedding structure tree
    protected KahinaWindow embeddingWindow;
    
    private int windowID;
    
    public KahinaWindow(KahinaWindowManager wm)
    {        
    	this.wm = wm;
    	windowID = idCounter++;
        setLayout(new BorderLayout());
        mainPanel = new KahinaTransferablePanel(this.getTitle(), windowID);
        mainPanel.addMouseListener(new KahinaWindowListener(this));
        embeddingWindow = null;
        this.add(mainPanel);
        //TODO: find a way to make windows more compact and to avoid having the title twice
        //this.setUndecorated(true);
        this.addWindowListener(this);
    }
    
    public int getID()
    {
    	return windowID;
    }
    
    public void setTitle(String title)
    {
    	super.setTitle(title);
    	mainPanel.setTitle(title + " (" + windowID + ")");
    }
    
    public boolean isTopLevelWindow()
    {
    	return wm.isTopLevelWindow(this);
    }
    
    //per default, a window is not considered a content window
    public boolean isContentWindow()
    {
    	return false;
    }
    
    //per default, a window is not flippable
    public boolean isFlippableWindow()
    {
    	return false;
    }
    
    //only KahinaDummyWindow returns true by default
    public boolean isDummyWindow()
    {
    	return false;
    }
    
    public void flipSubwindows()
    {
    	//do nothing per default, implemented by KahinaHorizontallySplitWindow and KahinaVerticallySplitWindow
    }
    
    public KahinaWindow getEmbeddingWindow()
    {
    	return embeddingWindow;
    }
    
    //for a container window, releases a subwindow and provides a replacement without the removed subwindow
    public KahinaWindow getReplacementAfterRelease(KahinaWindow subwindow)
    {
    	return this;
    }
    
    //for a container window, replaces a subwindow with the other
    public void replaceSubwindow(KahinaWindow oldSubwindow, KahinaWindow newSubwindow)
    {

    }

	@Override
	public void windowActivated(WindowEvent e) 
	{	
	}

	@Override
	public void windowClosed(WindowEvent e) 
	{	
	}

	@Override
	public void windowClosing(WindowEvent e) 
	{
		wm.control.processEvent(new KahinaWindowEvent(KahinaWindowEventType.TOGGLE_VISIBLE,this.getID()));
	}

	@Override
	public void windowDeactivated(WindowEvent e) 
	{	
	}

	@Override
	public void windowDeiconified(WindowEvent e) 
	{	
	}

	@Override
	public void windowIconified(WindowEvent e) 
	{		
	}

	@Override
	public void windowOpened(WindowEvent e) 
	{		
	}   
}
