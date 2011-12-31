package org.kahina.core.gui;

import java.awt.Container;
import java.awt.dnd.DropTarget;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JMenuBar;

import org.kahina.core.KahinaRunner;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.event.KahinaEventTypes;
import org.kahina.core.event.KahinaSessionEvent;
import org.kahina.core.event.KahinaSystemEvent;
import org.kahina.core.event.KahinaTreeEvent;
import org.kahina.core.event.KahinaTreeEventType;

public class KahinaMainWindow extends KahinaWindow implements KahinaListener
{
	private static final long serialVersionUID = 4400677323996243739L;
	
	private static final boolean VERBOSE = false;

	public static boolean verbose = false;

	protected JMenuBar menuBar;

	KahinaWindow subwindow;

	public KahinaMainWindow(KahinaWindowManager windowManager)
	{
		super(windowManager);
		this.initializeMainWindow();
	}

	public KahinaMainWindow(KahinaWindowManager windowManager, int winID)
	{
		super(windowManager, winID);
		this.initializeMainWindow();
	}

	private void initializeMainWindow()
	{
		this.setTitle("Kahina");
		wm.control.registerListener(KahinaEventTypes.TREE, this);
		// this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		// Uncomment this in order to be able to profile using JRat.

		menuBar = new JMenuBar();
		menuBar.add(new KahinaSessionMenu());
		menuBar.add(new KahinaViewMenu(wm));

		addAdditionalMenus();

		menuBar.add(new KahinaHelpMenu());
		this.setJMenuBar(menuBar);

		mainPanel.setDropTarget(new DropTarget(mainPanel, new KahinaDropTargetListener(this)));

		wm.control.registerListener(KahinaEventTypes.SYSTEM, this);
		wm.control.registerListener(KahinaEventTypes.SESSION, this);
	}

	public void setSize(int width, int height)
	{
		super.setSize(width, height);
		mainPanel.setSize(width - 10, height - 50);
	}

	protected void addAdditionalMenus()
	{

	}

	public boolean addSubwindow(KahinaWindow w)
	{
		if (subwindow == null)
		{
			setSubwindow(w);
			return true;
		} else
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
		wm.arr.setEmbeddingWindowID(w.getID(), windowID);
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
			wm.arr.setEmbeddingWindowID(removedWindow.getID(), -1);

			// crudely determine not too surprising positions and sizes for the
			// separate windows
			removedWindow.setSize(this.getWidth(), this.getHeight() - 30);
			removedWindow.setLocation(this.getX() + 30, this.getY() + 50);

			removedWindow.setContentPane((Container) mainPanel.getComponents()[0]);
			subwindow = null;
		} else
		{
			System.err.println("WARNING: Window \"" + removedWindow.getTitle() + "\" is not embedded directly under the main window, release failed.");
		}
		return this;
	}

	public void replaceSubwindow(KahinaWindow oldSubwindow, KahinaWindow newSubwindow)
	{
		if (subwindow == oldSubwindow)
		{
			wm.arr.setEmbeddingWindowID(oldSubwindow.getID(), -1);

			setSubwindow(newSubwindow);
		} else
		{
			System.err.println("WARNING: Window \"" + oldSubwindow.getTitle() + "\" not found as a tab in window \"" + this.getTitle() + "\", replacement failed.");
		}
	}

	@Override
	public void processEvent(KahinaEvent event)
	{
		if (event instanceof KahinaTreeEvent)
		{
			processTreeEvent((KahinaTreeEvent) event);
		} else if (event instanceof KahinaSystemEvent)
		{
			processSystemEvent((KahinaSystemEvent) event);
		} else if (event instanceof KahinaSessionEvent)
		{
			processSessionEvent((KahinaSessionEvent) event);
		}
	}

	private void processSessionEvent(KahinaSessionEvent event)
	{
		if (event.getSessionEventType() == KahinaSessionEvent.LOAD_SESSION)
		{
			// TODO This is a kludge, see below.
			setTitle("Kahina");
		}
	}

	private void processTreeEvent(KahinaTreeEvent event)
	{
		if (event.getTreeEventType() == KahinaTreeEventType.NEW_NODE)
		{
			// TODO This is a kludge, we should synchronize the step
			// count with the state. But first, the architecture needs to be
			// changed to allow access to the state.
			setTitle("Kahina (" + event.getFirstID() + ")");
		}
	}

	private void processSystemEvent(KahinaSystemEvent event)
	{
		if (event.getSystemEventType() == KahinaSystemEvent.QUIT)
		{
			disposeAllWindows();
		}
	}
}
