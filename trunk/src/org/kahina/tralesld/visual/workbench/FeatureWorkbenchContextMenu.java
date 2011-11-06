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
		
		JMenuItem removeItem = new JMenuItem("Remove");
		removeItem.addActionListener(viewPanel);
		add(removeItem);
		
		JMenuItem copyItem = new JMenuItem("Copy");
		copyItem.addActionListener(viewPanel);
		add(copyItem);
		
		JMenuItem pasteItem = new JMenuItem("Paste");
		pasteItem.addActionListener(viewPanel);
		add(pasteItem);
	}
}
