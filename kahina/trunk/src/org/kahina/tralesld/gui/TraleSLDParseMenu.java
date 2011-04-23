package org.kahina.tralesld.gui;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.core.KahinaInstance;
import org.kahina.tralesld.TraleSLDInstance;

public class TraleSLDParseMenu extends JMenu
{
	private static final long serialVersionUID = -1290849167486564257L;

	public TraleSLDParseMenu(KahinaInstance<?, ?, ?> kahina)
	{
		super("Parse");
		TraleSLDInstance instance = (TraleSLDInstance) kahina;
		add(new JMenuItem(instance.COMPILE_ACTION));
		add(new JMenuItem(instance.PARSE_ACTION));
		add(new JMenuItem(instance.RESTART_ACTION));
	}
}
