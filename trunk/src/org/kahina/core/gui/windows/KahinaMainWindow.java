package org.kahina.core.gui.windows;

import java.awt.Container;
import java.awt.Toolkit;
import java.awt.dnd.DropTarget;

import javax.swing.ImageIcon;
import javax.swing.JMenuBar;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.control.KahinaSystemEvent;
import org.kahina.core.data.project.KahinaProjectStatus;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.gui.menus.KahinaHelpMenu;
import org.kahina.core.gui.menus.KahinaSessionMenu;
import org.kahina.core.gui.menus.KahinaViewMenu;
import org.kahina.core.gui.menus.KahinaProjectMenu;
import org.kahina.core.io.util.IconUtil;

public class KahinaMainWindow extends KahinaWindow implements KahinaListener
{
	private static final long serialVersionUID = 4400677323996243739L;
	
	private static final boolean VERBOSE = false;

	public static boolean verbose = false;

	protected JMenuBar menuBar;
    
    protected KahinaSessionMenu sessionMenu;
    protected KahinaProjectMenu projectMenu;
    protected KahinaViewMenu viewMenu;

	KahinaWindow subwindow;

	public KahinaMainWindow(KahinaWindowManager windowManager, KahinaInstance<?, ?, ?, ?> kahina)
	{
		super(windowManager, kahina);
		this.initializeMainWindow();
	}

	public KahinaMainWindow(KahinaWindowManager windowManager, KahinaInstance<?, ?, ?, ?> kahina, int winID)
	{
		super(windowManager, kahina, winID);
		this.initializeMainWindow();
	}

	private void initializeMainWindow()
	{
		this.setTitle(kahina.getApplicationName());
		this.setIconImage(new ImageIcon(IconUtil.getIcon("gui/icons/logo.png")).getImage());
		// Uncomment this in order to be able to profile using JRat.
        // this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		menuBar = new JMenuBar();
        
        projectMenu = new KahinaProjectMenu(kahina);
        menuBar.add(projectMenu);
        
        //TODO: reactivate this menu and the functionality it once provided
        //sessionMenu = new KahinaSessionMenu(kahina);
        //menuBar.add(sessionMenu);
		
		addMenusInFront();

		if (showsViewMenu())
		{
		    viewMenu = new KahinaViewMenu(kahina);
		    menuBar.add(viewMenu);
		}

		addMenusBeforeHelpMenu();

		menuBar.add(new KahinaHelpMenu(kahina));
		this.setJMenuBar(menuBar);

		mainPanel.setDropTarget(new DropTarget(mainPanel, new KahinaDropTargetListener(this)));

		kahina.registerInstanceListener(KahinaEventTypes.SYSTEM, this);
	}
	
	public void deregister()
	{
	    if (viewMenu != null) viewMenu.deregister();
	    kahina.deregisterInstanceListener(KahinaEventTypes.SYSTEM, this);
	}

	public void setSize(int width, int height)
	{
		super.setSize(width, height);
		mainPanel.setSize(width - 10, height - 50);
	}
	
	protected void addMenusInFront()
	{

	}

	protected void addMenusBeforeHelpMenu()
	{

	}

	public boolean addSubwindow(KahinaWindow w)
	{
		if (subwindow == null)
		{
			setSubwindow(w);
			return true;
		} 
		else
		{
			return false;
		}
	}

	public void setSubwindow(KahinaWindow w)
	{
		if (VERBOSE)
		{
			System.err.println("Setting subwindow: " + w.getID());
		}
		wm.getArrangement().setEmbeddingWindowID(w.getID(), windowID);
		subwindow = w;
		mainPanel.removeAll();
		mainPanel.add(w.getContentPane());
	}

	public int getWindowType()
	{
		return KahinaWindowType.MAIN_WINDOW;
	}

	private void disposeAllWindows()
	{
		wm.disposeAllWindows();
	}

	public KahinaWindow getReplacementAfterRelease(KahinaWindow removedWindow)
	{
		if (subwindow == removedWindow)
		{
			wm.getArrangement().setEmbeddingWindowID(removedWindow.getID(), -1);

			// crudely determine not too surprising positions and sizes for the
			// separate windows
			removedWindow.setSize(this.getWidth(), this.getHeight() - 30);
			removedWindow.setLocation(this.getX() + 30, this.getY() + 50);

			removedWindow.setContentPane((Container) mainPanel.getComponents()[0]);
			subwindow = null;
		} 
		else
		{
			System.err.println("WARNING: Window \"" + removedWindow.getTitle() + "\" is not embedded directly under the main window, release failed.");
		}
		return this;
	}

	public void replaceSubwindow(KahinaWindow oldSubwindow, KahinaWindow newSubwindow)
	{
		if (subwindow == oldSubwindow)
		{
			wm.getArrangement().setEmbeddingWindowID(oldSubwindow.getID(), -1);

			setSubwindow(newSubwindow);
		} 
		else
		{
			System.err.println("WARNING: Window \"" + oldSubwindow.getTitle() + "\" not found as a tab in window \"" + this.getTitle() + "\", replacement failed.");
		}
	}
    
    public void processProjectStatus(KahinaProjectStatus projectStatus)
    {
        switch (projectStatus)
        {
            case NO_OPEN_PROJECT:
            {
                break;
            }
            case PROGRAM_UNCOMPILED:
            {
                break;
            }
            case PROGRAM_COMPILED:
            {
                break;
            }
        }
    }

	@Override
	public void processEvent(KahinaEvent event)
	{
		if (event instanceof KahinaSystemEvent)
		{
			processSystemEvent((KahinaSystemEvent) event);
		}
	}

	private void processSystemEvent(KahinaSystemEvent event)
	{
		if (event.getSystemEventType() == KahinaSystemEvent.NODE_COUNT)
		{
		    if (kahina.getProject() == null)
		    {
		        setTitle(kahina.getApplicationName());
		    }
		    else
		    {
		        setTitle(kahina.getApplicationName() + " - " + kahina.getProject().getName() + " (" + event.getIntContent() + ")");
		    }
		} 
        else if (event.getSystemEventType() == KahinaSystemEvent.QUIT)
		{
			disposeAllWindows();
		}
	}
	
	protected boolean showsViewMenu()
	{
	    return true;
	}
}
