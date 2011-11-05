package org.kahina.tralesld.visual.workbench;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

public class FeatureWorkbenchContextMenu extends JPopupMenu
{
	public FeatureWorkbenchContextMenu(FeatureWorkbenchViewPanel viewPanel)
	{
		super();
		
		JMenuItem renameItem = new JMenuItem("Rename");
		renameItem.addActionListener(viewPanel);
		add(renameItem);
	}
}
