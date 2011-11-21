package org.kahina.tralesld.visual.fs;

import java.util.List;
import java.util.Set;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

public class TraleSLDFeatureStructureEditorMenu extends JPopupMenu
{
	public TraleSLDFeatureStructureEditorMenu()
	{
		super();
	}
	
	public static TraleSLDFeatureStructureEditorMenu newTypeMenu(TraleSLDFeatureStructureEditor editor,
			List<String> subtypes, List<String> supertypes, List<String> siblingTypes,
			List<String> features, boolean totallyWellTyped)
	{
		TraleSLDFeatureStructureEditorMenu menu = new TraleSLDFeatureStructureEditorMenu();
		
		JMenu specializeMenu = new JMenu("Specialize to");
		if (subtypes.size() == 0)
		{
			specializeMenu.setEnabled(false);
		}
		else for (String type : subtypes)
		{
			JMenuItem typeItem = new JMenuItem(type);
			typeItem.setActionCommand("spe:" + type);
			typeItem.addActionListener(editor);
			specializeMenu.add(typeItem);
		}
		menu.add(specializeMenu);
		
		JMenu generalizeMenu = new JMenu("Generalize to");
		if (supertypes.size() == 0)
		{
			generalizeMenu.setEnabled(false);
		}
		else for (String type : supertypes)
		{
			JMenuItem typeItem = new JMenuItem(type);
			typeItem.setActionCommand("gen:" + type);
			typeItem.addActionListener(editor);
			generalizeMenu.add(typeItem);
		}
		menu.add(generalizeMenu);
		
		JMenu switchMenu = new JMenu("Switch to");
		if (siblingTypes.size() == 0)
		{
			switchMenu.setEnabled(false);
		}
		else for (String type : siblingTypes)
		{
			JMenuItem typeItem = new JMenuItem(type);
			typeItem.setActionCommand("swi:" + type);
			typeItem.addActionListener(editor);
			switchMenu.add(typeItem);
		}
		menu.add(switchMenu);
		
		menu.addSeparator();
		
		if (!totallyWellTyped)
		{
			JMenu featMenu = new JMenu("Add feature");
			if (features.size() == 0)
			{
				featMenu.setEnabled(false);
			}
			else for (String feat : features)
			{
				JMenuItem featItem = new JMenuItem(feat);
				featItem.setActionCommand("fea:" + feat);
				featItem.addActionListener(editor);
				featMenu.add(featItem);
			}
			menu.add(featMenu);
			menu.addSeparator();
		}
		
		JMenuItem copyItem = new JMenuItem("Copy");
		copyItem.addActionListener(editor);
		menu.add(copyItem);
		
		JMenuItem pasteItem = new JMenuItem("Paste");
		pasteItem.addActionListener(editor);
		if (editor.getBufferedStructure() == null)
		{
			pasteItem.setEnabled(false);
		}
		menu.add(pasteItem);
		return menu;
	}
}
