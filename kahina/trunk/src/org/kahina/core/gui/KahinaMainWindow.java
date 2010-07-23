package org.kahina.core.gui;

import java.awt.BorderLayout;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JMenuBar;

import org.kahina.core.KahinaRunner;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.event.KahinaEventTypes;
import org.kahina.core.event.KahinaSystemEvent;
import org.kahina.core.event.KahinaTreeEvent;
import org.kahina.core.event.KahinaTreeEventType;

public class KahinaMainWindow extends KahinaWindow implements KahinaListener
{
	private static final long serialVersionUID = 4400677323996243739L;

	public static boolean verbose = false;

	public KahinaWindowManager windowManager;
	
	private int stepCount = 0;

	public KahinaMainWindow(KahinaWindowManager windowManager)
	{
		this.windowManager = windowManager;

		this.setTitle("Kahina");
		KahinaRunner.getControl().registerListener(KahinaEventTypes.TREE, this);
		this.setLayout(new BorderLayout());
		// this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		// Uncomment this in order to be able to profile using JRat.

		JMenuBar menuBar = new JMenuBar();
		menuBar.add(new KahinaSessionMenu());
		menuBar.add(new KahinaParseMenu());
		menuBar.add(new KahinaBreakpointMenu());
		menuBar.add(new KahinaHelpMenu());
		this.setJMenuBar(menuBar);

		windowManager.gui.getControlPanel().build();
		this.add(windowManager.gui.getControlPanel(), BorderLayout.PAGE_START);

		// TODO: adapt this to the size of the control panel
		int width = 625;
		int height = 120;
		this.setSize(width, height);

		setVisible(true);

		this.validate();

		KahinaRunner.getControl().registerListener("system", this);
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
			KahinaTreeEvent treeEvent = (KahinaTreeEvent) event;

			if (treeEvent.getTreeEventType() == KahinaTreeEventType.NEW_NODE)
			{
				incrementStepCount();
			}
		} else if (event instanceof KahinaSystemEvent)
		{
			processSystemEvent((KahinaSystemEvent) event);
		}
	}

	private void processSystemEvent(KahinaSystemEvent event)
	{
		if (event.getSystemEventType() == KahinaSystemEvent.QUIT)
		{
			disposeAllWindows();
		}
	}

	private void incrementStepCount()
	{
		stepCount++;
		setTitle("Kahina (" + stepCount + ")");
	}
}
