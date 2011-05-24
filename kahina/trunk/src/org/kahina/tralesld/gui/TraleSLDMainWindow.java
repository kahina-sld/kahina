package org.kahina.tralesld.gui;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaMainWindow;
import org.kahina.core.gui.KahinaWindowManager;

public class TraleSLDMainWindow extends KahinaMainWindow
{
	private static final long serialVersionUID = -8044329699904664157L;

	public TraleSLDMainWindow(KahinaWindowManager windowManager, KahinaInstance<?, ?, ?> kahina)
	{
		super(windowManager, kahina);
		menuBar.add(new TraleSLDParseMenu(kahina), 2);
	}
	
	public TraleSLDMainWindow(KahinaWindowManager windowManager, KahinaInstance<?, ?, ?> kahina, int winID)
	{
		super(windowManager, kahina, winID);
		menuBar.add(new TraleSLDParseMenu(kahina), 2);
	}

}
