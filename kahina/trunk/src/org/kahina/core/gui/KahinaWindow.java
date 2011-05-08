package org.kahina.core.gui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;
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
    
    protected final int windowID;
    protected boolean cloned;
    
    /**
     * Constructs a KahinaWindow with a new unique ID.
     * Caution is advised with this constructor, as incorrect ID assignment can break the window system.
     * @param wm the window manager that is to manage this window
     */
    public KahinaWindow(KahinaWindowManager wm)
    {     
    	this.wm = wm;
    	windowID = idCounter++;
        setLayout(new BorderLayout());
        mainPanel = new KahinaTransferablePanel(this.getTitle(), windowID);
        mainPanel.addMouseListener(new KahinaWindowListener(this));
        wm.arr.setEmbeddingWindowID(windowID, -1);
        cloned = false;
        this.add(mainPanel);
        //TODO: find a way to make windows more compact and to avoid having the title twice
        //this.setUndecorated(true);
        this.addWindowListener(this);
        wm.registerWindow(this);
    }
    
    /**
     * Constructs a KahinaWindow with a specified ID.
     * Caution is advised with this constructor, as incorrect ID assignment can break the window system.
     * @param wm the window manager that is to manage this window
     * @param id the unique window ID that this window will be referred by (never use -1 or an ID that is already used!)
     */
    public KahinaWindow(KahinaWindowManager wm, int id)
    {     
    	this.wm = wm;
    	this.windowID = id;
    	//make sure the other constructor does not cause any ID clashes
    	if (windowID > idCounter)
    	{
    		idCounter = windowID + 1;
    	}
        setLayout(new BorderLayout());
        mainPanel = new KahinaTransferablePanel(this.getTitle(), windowID);
        mainPanel.addMouseListener(new KahinaWindowListener(this));
        wm.arr.setEmbeddingWindowID(windowID, -1);
        cloned = false;
        this.add(mainPanel);
        //TODO: find a way to make windows more compact and to avoid having the title twice
        //this.setUndecorated(true);
        this.addWindowListener(this);
        wm.registerWindow(this);
    }
    
    public int getID()
    {
    	return windowID;
    }
    
    public boolean isClone()
    {
    	return cloned;
    }
    
    //overloaded property manipulation to also update the manager's arrangement object
    //this gives us adaptation of the arrangement object to resizes and movements for free
    //the problem is that some of these are called before the window manager is fully initialized
    
    public void setTitle(String title)
    {
    	super.setTitle(title);
    	mainPanel.setTitle(title + " (" + windowID + ")");
    	wm.arr.setTitle(windowID,title);
    }
    
    public void setSize(int width, int height)
    {
    	super.setSize(width,height);
    	wm.arr.setSize(windowID,width,height);
    }
    
    public void setSize(Dimension size)
    {
    	super.setSize(size);
    	wm.arr.setSize(windowID,size.width,size.height);
    }
    
    public void setLocation(int xPos, int yPos)
    {
    	super.setLocation(xPos,yPos);
    	if (wm != null)
    	{
    		wm.arr.setXPos(windowID,xPos);
    		wm.arr.setYPos(windowID,yPos);
    	}
    }
    
    public void setLocation(Point p)
    {
    	super.setLocation(p);
    	wm.arr.setXPos(windowID, p.x);
    	wm.arr.setYPos(windowID, p.y);
    }
    
    public void setBounds(int xPos, int yPos, int width, int height)
    {
    	super.setBounds(xPos,yPos,width,height);
    	if (wm != null)
    	{
    		wm.arr.setXPos(windowID, xPos);
    		wm.arr.setYPos(windowID, yPos);
    		wm.arr.setSize(windowID,width,height);
    	}
    }
    
    public void setBounds(Rectangle rect)
    {
    	super.setBounds(rect);
    	wm.arr.setXPos(windowID, rect.x);
    	wm.arr.setYPos(windowID, rect.y);
    	wm.arr.setSize(windowID, rect.width, rect.height);
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
    
    /**
     * Creates an exact copy of this window, with identical update behavior.
     * Implementations must override this to provide the desired functionality.
     * 
     * @return an exact copy of the current window, with identical behavior
     */
    public KahinaWindow createDynamicClone()
    {
    	return new KahinaWindow(wm);
    }
    
    /**
     * Creates an exact copy of this window, but with immutable content.
     * Implementations must override this to provide the desired functionality.
     * 
     * @return an copy of the current window at the current step, with immutable content
     */
    public KahinaWindow createSnapshotClone()
    {
    	return new KahinaWindow(wm);
    }
    
    /**
     * 
     * @return the embedding KahinaWindow; null if it this is a top-level window
     */
    public KahinaWindow getEmbeddingWindow()
    {
    	return wm.getWindowByID(wm.arr.getEmbeddingWindowID(windowID));
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
