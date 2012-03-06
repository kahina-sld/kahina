package org.kahina.core.gui.profiler;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenu;

import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaDialogEvent;
import org.kahina.core.util.SwingUtil;

public class KahinaProfilerMenu extends JMenu implements ActionListener
{
	private static final long serialVersionUID = -7484775954027995992L;
	private KahinaController guiControl;

	public KahinaProfilerMenu(KahinaController guiControl)
	{
		super("Profiler");
		this.guiControl = guiControl;
		
		add(SwingUtil.createMenuItem("Full profile", "fullProfile", this));
		add(SwingUtil.createMenuItem("Profile call subtree", "callSubtreeProfile", this));
		add(SwingUtil.createMenuItem("Profile search subtree", "searchSubtreeProfile", this));
		addSeparator();
		add(SwingUtil.createMenuItem("Edit warnings", "editWarnings", this));
	}

	@Override
	public void actionPerformed(ActionEvent e)
	{
		String command = e.getActionCommand();
		if (command.equals("fullProfile"))
		{
			guiControl.processEvent(new KahinaDialogEvent(KahinaDialogEvent.FULL_PROFILE));
		} else if (command.equals("callSubtreeProfile"))
		{
			guiControl.processEvent(new KahinaDialogEvent(KahinaDialogEvent.CALL_SUBTREE_PROFILE));
		} else if (command.equals("searchSubtreeProfile"))
		{
			guiControl.processEvent(new KahinaDialogEvent(KahinaDialogEvent.SEARCH_SUBTREE_PROFILE));
		} else if (command.equals("editWarnings"))
		{
			guiControl.processEvent(new KahinaDialogEvent(KahinaDialogEvent.EDIT_WARNINGS));
		}
	}

}
