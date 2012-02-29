package org.kahina.qtype.gui;

import org.kahina.lp.gui.LogicProgrammingMainWindow;
import org.kahina.parse.gui.ProjectMenu;

public class QTypeMainWindow extends LogicProgrammingMainWindow
{
	private static final long serialVersionUID = -8044329699904664157L;

	public QTypeMainWindow(QTypeWindowManager windowManager)
	{
		super(windowManager);
	}
	
	public QTypeMainWindow(QTypeWindowManager windowManager, int winID)
	{
		super(windowManager, winID);
	}
	
	@Override
	protected void addMenusInFront()
	{
		menuBar.add(new ProjectMenu(wm.gui.getKahinaInstance()));
		//TODO: define and add a good ProjectMenuListener
		menuBar.add(new QTypeParseMenu(wm.gui.getKahinaInstance()));
	}

}
