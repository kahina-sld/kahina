package org.kahina.qtype.gui;

import org.kahina.core.KahinaInstance;
import org.kahina.core.gui.menus.ProjectMenu;
import org.kahina.lp.gui.LogicProgrammingMainWindow;

public class QTypeMainWindow extends LogicProgrammingMainWindow
{
	private static final long serialVersionUID = -8044329699904664157L;

	public QTypeMainWindow(QTypeWindowManager windowManager, KahinaInstance<?, ?, ?> kahina)
	{
		super(windowManager, kahina);
	}
	
	public QTypeMainWindow(QTypeWindowManager windowManager, KahinaInstance<?, ?, ?> kahina, int winID)
	{
		super(windowManager, kahina, winID);
	}
	
	@Override
	protected void addMenusInFront()
	{
		menuBar.add(new ProjectMenu(kahina));
		//TODO: define and add a good ProjectMenuListener
		menuBar.add(new QTypeParseMenu(kahina));
	}

}
