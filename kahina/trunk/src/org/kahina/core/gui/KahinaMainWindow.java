package org.kahina.core.gui;

import java.awt.BorderLayout;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JMenuBar;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaRunner;
import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.event.KahinaEventTypes;
import org.kahina.core.event.KahinaSessionEvent;
import org.kahina.core.event.KahinaSystemEvent;
import org.kahina.core.event.KahinaTreeEvent;
import org.kahina.core.event.KahinaTreeEventType;
import org.kahina.core.gui.profiler.KahinaProfilerMenu;

public class KahinaMainWindow extends KahinaWindow implements KahinaListener
{
	private static final long serialVersionUID = 4400677323996243739L;

	public static boolean verbose = false;

	public KahinaWindowManager windowManager;
	
	protected JMenuBar menuBar;

	public KahinaMainWindow(KahinaWindowManager windowManager, KahinaController control, KahinaInstance<?, ?, ?> kahina)
	{
		super(windowManager);
		this.windowManager = windowManager;

		this.setTitle("Kahina");
		control.registerListener(KahinaEventTypes.TREE, this);
		this.setLayout(new BorderLayout());
		// this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		// Uncomment this in order to be able to profile using JRat.

		menuBar = new JMenuBar();
		menuBar.add(new KahinaSessionMenu());
		menuBar.add(new KahinaViewMenu(windowManager));
		
		// TODO these three menus are specific to logic programming, should be added in subclasses
		menuBar.add(new KahinaBreakpointMenu());
		menuBar.add(new KahinaProfilerMenu());
		
		menuBar.add(new KahinaHelpMenu());
		this.setJMenuBar(menuBar);

		windowManager.gui.getControlPanel().build();
		this.add(windowManager.gui.getControlPanel(), BorderLayout.PAGE_START);

		// TODO: adapt this to the size of the control panel
		int width = 625;
		int height = 120;
		this.setSize(width, height);

		control.registerListener(KahinaEventTypes.SYSTEM, this);
		control.registerListener(KahinaEventTypes.SESSION, this);
		this.addWindowListener(new WindowAdapter()
		{
			@Override
			public void windowClosing(WindowEvent e)
			{
				KahinaRunner.processEvent(new KahinaSystemEvent(KahinaSystemEvent.QUIT));
			}

		});
	}

	private void disposeAllWindows()
	{
		windowManager.disposeAllWindows();
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
