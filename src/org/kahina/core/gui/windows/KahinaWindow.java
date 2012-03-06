package org.kahina.core.gui.windows;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.BorderFactory;
import javax.swing.JFrame;

import org.kahina.core.KahinaRunner;
import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaSystemEvent;
import org.kahina.core.gui.KahinaTransferablePanel;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.gui.event.KahinaWindowEvent;
import org.kahina.core.gui.event.KahinaWindowEventType;

public class KahinaWindow extends JFrame implements WindowListener, ComponentListener
{
	private static final long serialVersionUID = 6613805267152521669L; 
    private static final boolean VERBOSE = false; 
    
    private static int idCounter = 0;
    
    public KahinaWindowManager wm;
    protected KahinaTransferablePanel mainPanel;
    
    protected final int windowID;
    protected boolean cloned;
    
    protected KahinaController control;
    
    /**
     * Constructs a KahinaWindow with a new unique window ID.
     * @param wm the window manager that is to manage this window
     */
    public KahinaWindow(KahinaWindowManager wm, KahinaController control)
    {     
    	this.wm = wm;
    	windowID = idCounter++;
    	this.control = control;
        initialize();
    }
    
    /**
     * Constructs a KahinaWindow with a specified window ID.
     * Caution is advised with this constructor, as duplicate ID assignment can break the window system.
     * @param wm the window manager that is to manage this window
     * @param id the unique window ID that this window will be referred by (never use -1 or an ID that is already used!)
     */
    public KahinaWindow(KahinaWindowManager wm, KahinaController control, int id)
    {
    	if (VERBOSE)
    	{
    		System.err.println(this + "(" + wm + ", " + id + ")");
    	}
    	this.wm = wm;
    	this.windowID = id;
    	this.control = control;
    	//make sure the other constructor does not cause any ID clashes
    	if (windowID >= idCounter)
    	{
    		idCounter = windowID + 1;
    	}
    	initialize();
    }
    
    private void initialize()
    {
        setLayout(new BorderLayout());
        mainPanel = new KahinaTransferablePanel(this.getTitle(), windowID);
        mainPanel.addMouseListener(new KahinaWindowListener(this));
        cloned = false;
        this.getContentPane().add(mainPanel);
        this.addWindowListener(this);
        this.addComponentListener(this);
        wm.registerWindow(this);
        
        // Send a Quit system event when the main window or a window containing it is closed.
        addWindowListener(new WindowAdapter()
        {

			@Override
			public void windowClosing(WindowEvent e)
			{
				//TODO: find out what condition KahinaRunner.isInitialized() did here
				if (containsMainWindow())
				{
					control.processEvent(new KahinaSystemEvent(KahinaSystemEvent.QUIT));					
				}
			}
			
		});
    }

	private boolean containsMainWindow()
	{
		if (this instanceof KahinaMainWindow)
		{
			return true;
		}
		
		for (Component component : getComponents())
		{
			if (component instanceof KahinaWindow)
			{
				if (((KahinaWindow) component).containsMainWindow())
				{
					return true;
				}
			}
		}
		
		return false;
	}
    
    public int getID()
    {
    	return windowID;
    }
    
    public boolean isClone()
    {
    	return cloned;
    }
    
    public void setBorder(boolean border)
    {
    	wm.getArrangement().setBorder(windowID, border);
    	if (border)
    	{
        	mainPanel.setBorder(BorderFactory.createTitledBorder(mainPanel.getTitle()));
    	}
    	else
    	{
    		mainPanel.setBorder(null);
    	}
    }
    
    /**
     * Returns the type of the current window. Used in storing arrangements.
     * Returns KahinaWindowType.DEFAULT_WINDOW by default, indicating no embedded windows.
     * This is overridden by specializations that embed other windows.
     * @return one of the constants defined in KahinaWindowType, representing the type of this window
     */
	public int getWindowType()
	{
		return KahinaWindowType.DEFAULT_WINDOW;
	}
    
    public void setTitle(String title)
    {
    	if (VERBOSE)
    	{
    		System.err.println(this + ".setTitle(" + title + ")");
    	}
    	super.setTitle(title);
    	mainPanel.setTitle(title);
    	//mainPanel.setTitle(title + " (" + windowID + ")");
    	wm.getArrangement().setTitle(windowID, title);
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
    
    public void setSize(int width, int height)
    {
        super.setSize(width,height);
        wm.getArrangement().setSize(windowID,this.getWidth(),this.getHeight());
    }
    
    public void repaintMainPanel()
    {
    	mainPanel.repaint();
    }
    
    /**
     * Creates an exact copy of this window, with identical update behavior.
     * Implementations must override this to provide the desired functionality.
     * 
     * @return an exact copy of the current window, with identical behavior
     */
    public KahinaWindow createDynamicClone()
    {
    	return new KahinaWindow(wm, control);
    }
    
    /**
     * Creates an exact copy of this window, but with immutable content.
     * Implementations must override this to provide the desired functionality.
     * 
     * @return an copy of the current window at the current step, with immutable content
     */
    public KahinaWindow createSnapshotClone()
    {
    	return new KahinaWindow(wm, control);
    }
    
    /**
     * Retrieves the direct ancestor of this window in the embedding tree.
     * @return the embedding KahinaWindow; null if it this is a top-level window
     */
    public KahinaWindow getEmbeddingWindow()
    {
    	return wm.getWindowByID(wm.getArrangement().getEmbeddingWindowID(windowID));
    }
    
    /**
     * Adds a subwindow if there is space for it; generic handle for configuring windows.
     * @param subwindow the subwindow to be added
     * @return whether the subwindow was successfully added; false by default
     */
    public boolean addSubwindow(KahinaWindow subwindow)
    {
    	return false;
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
		wm.getGuiControl().processEvent(new KahinaWindowEvent(KahinaWindowEventType.TOGGLE_VISIBLE,this.getID()));
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

	@Override
	public void componentHidden(ComponentEvent arg0) 
	{
		
	}

	@Override
	public void componentMoved(ComponentEvent arg0) 
	{
		wm.getArrangement().setXPos(windowID, this.getX());
		wm.getArrangement().setYPos(windowID, this.getY());
	}

	@Override
	public void componentResized(ComponentEvent arg0) 
	{
        //System.err.println("KahinaWindow " + windowID + " resized to (" + this.getWidth() + "," + this.getHeight() + ")");
		wm.getArrangement().setSize(windowID,this.getWidth(),this.getHeight());
	}

	@Override
	public void componentShown(ComponentEvent arg0) 
	{
		
	}   
	
	public String toString()
	{
		return getTitle();
	}
}
