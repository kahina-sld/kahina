package org.kahina.core.gui.profiler;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenu;

import org.kahina.core.util.KahinaSwingUtilities;

public class KahinaProfilerMenu extends JMenu implements ActionListener
{
	
	private static final long serialVersionUID = -7484775954027995992L;

	public KahinaProfilerMenu()
	{
		super("Profiler");
		add(KahinaSwingUtilities.createMenuItem("Full profile", "fullProfile", this));
		add(KahinaSwingUtilities.createMenuItem("Call subtree profile", "callSubtreeProfile", this));
		add(KahinaSwingUtilities.createMenuItem("Search subtree profile", "searchSubtreeProfile", this));
		addSeparator();
		add(KahinaSwingUtilities.createMenuItem("Edit warnings", "editWarnings", this));
	}

	@Override
	public void actionPerformed(ActionEvent e)
	{
		// TODO Auto-generated method stub
		
	}

}
