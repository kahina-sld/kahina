package org.kahina.tralesld.visual.fs;

import java.util.List;
import java.util.Set;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

public class TraleSLDFeatureStructureEditorMenu extends JPopupMenu
{
	public TraleSLDFeatureStructureEditorMenu(TraleSLDFeatureStructureEditor editor,
			Set<String> subtypes, Set<String> supertypes, Set<String> siblingTypes)
	{
		super();
		JMenu specializeMenu = new JMenu("Specialize to");
		for (String type : subtypes)
		{
			JMenuItem typeItem = new JMenuItem(type);
			typeItem.addActionListener(editor);
			specializeMenu.add(typeItem);
		}
		add(specializeMenu);
		JMenu generalizeMenu = new JMenu("Generalize to");
		for (String type : supertypes)
		{
			JMenuItem typeItem = new JMenuItem(type);
			typeItem.addActionListener(editor);
			generalizeMenu.add(typeItem);
		}
		add(generalizeMenu);
		JMenu switchMenu = new JMenu("Switch to");
		for (String type : siblingTypes)
		{
			JMenuItem typeItem = new JMenuItem(type);
			typeItem.addActionListener(editor);
			switchMenu.add(typeItem);
		}
		add(switchMenu);
	}
}
