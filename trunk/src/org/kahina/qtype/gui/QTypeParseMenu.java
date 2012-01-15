package org.kahina.qtype.gui;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.core.KahinaInstance;
import org.kahina.qtype.QTypeCommander;
import org.kahina.qtype.QTypeDebuggerInstance;

public class QTypeParseMenu extends JMenu
{
	private static final long serialVersionUID = -1290849167486564257L;

	public QTypeParseMenu(KahinaInstance<?, ?, ?> kahina)
	{
		super("Grammar");
		QTypeCommander commander = ((QTypeDebuggerInstance) kahina).getCommander();
		add(new JMenuItem(commander.COMPILE_ACTION));
		add(new JMenuItem(commander.PARSE_ACTION));
		add(new QTypeParseExampleMenu());
		add(new JMenuItem(commander.RESTART_ACTION));
	}
}
