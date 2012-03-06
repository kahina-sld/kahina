package org.kahina.tralesld.gui;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

public class TraleSLDWorkbenchMenu  extends JMenu
{
	private static final long serialVersionUID = 1091733910119223896L;

	public TraleSLDWorkbenchMenu(TraleSLDGUI gui)
	{
		super("Workbench");
		add(new JMenuItem(gui.NEW_WORKBENCH_ACTION));
	}
}