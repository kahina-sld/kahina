package org.kahina.core.gui.windows;

import java.awt.Container;
import java.awt.dnd.DropTarget;

import javax.swing.JMenuBar;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.control.KahinaSystemEvent;
import org.kahina.core.gui.KahinaSessionMenu;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.gui.menus.KahinaHelpMenu;
import org.kahina.core.gui.menus.KahinaViewMenu;

public class KahinaMainWindow extends KahinaWindow implements KahinaListener
{
	private static final long serialVersionUID = 4400677323996243739L;
	
	private static final boolean VERBOSE = false;

	public static boolean verbose = false;

	protected JMenuBar menuBar;

	KahinaWindow subwindow;

	public KahinaMainWindow(KahinaWindowManager windowManager, KahinaInstance<?, ?, ?> kahina)
	{
		super(windowManager, kahina);
		this.initializeMainWindow();
	}

	public KahinaMainWindow(KahinaWindowManager windowManager, KahinaInstance<?, ?, ?> kahina, int winID)
	{
		super(windowManager, kahina, winID);
		this.initializeMainWindow();
	}

	private void initializeMainWindow()
	{
		this.setTitle("Kahina");
		// this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		// Uncomment this in order to be able to profile using JRat.

		menuBar = new JMenuBar();
		
		addMenusInFront();
		
		menuBar.add(new KahinaSessionMenu(kahina.getControl()));
		menuBar.add(new KahinaViewMenu(wm));

		addMenusBeforeHelpMenu();

		menuBar.add(new KahinaHelpMenu(wm.getGuiControl()));
		this.setJMenuBar(menuBar);

		mainPanel.setDropTarget(new DropTarget(mainPanel, new KahinaDropTargetListener(this)));

		/**
		 * TODO This used to use getControl(), surely by mistake? Changed it to
		 * getGuiControl(), hopefully improving functionality rather than
		 * destroying it.
		 */
		kahina.getGuiControl().registerListener(KahinaEventTypes.SYSTEM, this);
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
			wm.getArrangement().setEmbeddingWindowID(oldSubwindow.getID(), -1);

			setSubwindow(newSubwindow);
		} else
		{
			System.err.println("WARNING: Window \"" + oldSubwindow.getTitle() + "\" not found as a tab in window \"" + this.getTitle() + "\", replacement failed.");
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
			setTitle("Kahina (" + event.getIntContent() + ")");
		} else if (event.getSystemEventType() == KahinaSystemEvent.QUIT)
		{
			disposeAllWindows();
		}
	}
}