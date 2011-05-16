package org.kahina.tralesld.gui;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaMainWindow;
import org.kahina.core.gui.KahinaWindowManager;

public class TraleSLDMainWindow extends KahinaMainWindow
{

	private static final long serialVersionUID = -8044329699904664157L;

	public TraleSLDMainWindow(KahinaWindowManager windowManager, KahinaController control, KahinaInstance<?, ?, ?> kahina)
	{
		super(windowManager, control, kahina);
		menuBar.add(new TraleSLDParseMenu(kahina), 2);
	}
	
	public TraleSLDMainWindow(KahinaWindowManager windowManager, KahinaController control, KahinaInstance<?, ?, ?> kahina, int winID)
	{
		super(windowManager, control, kahina, winID);
		menuBar.add(new TraleSLDParseMenu(kahina), 2);
	}

}
