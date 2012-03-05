package org.kahina.qtype.gui;

import org.kahina.core.control.KahinaController;
import org.kahina.lp.gui.LogicProgrammingMainWindow;
import org.kahina.parse.gui.ProjectMenu;

public class QTypeMainWindow extends LogicProgrammingMainWindow
{
	private static final long serialVersionUID = -8044329699904664157L;

	public QTypeMainWindow(QTypeWindowManager windowManager, KahinaController control)
	{
		super(windowManager, control);
	}
	
	public QTypeMainWindow(QTypeWindowManager windowManager, KahinaController control, int winID)
	{
		super(windowManager, control, winID);
	}
	
	@Override
	protected void addMenusInFront()
	{
		menuBar.add(new ProjectMenu(wm.kahina));
		//TODO: define and add a good ProjectMenuListener
		menuBar.add(new QTypeParseMenu(wm.kahina));
	}

}
