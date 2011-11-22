package org.kahina.tralesld.visual.fs;

import gralej.blocks.Block;
import gralej.blocks.BlockPanel;
import gralej.om.IEntity;
import gralej.om.IType;
import gralej.om.ITypedFeatureStructure;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import org.kahina.core.visual.chart.KahinaChartViewContextMenu;
import org.kahina.tralesld.data.signature.TraleSLDSignature;

public class TraleSLDFeatureStructureEditorMouseListener implements MouseListener
{
	TraleSLDFeatureStructureEditor editor;
	BlockPanel blockPanel;
	TraleSLDSignature sig;
	
	public TraleSLDFeatureStructureEditorMouseListener(TraleSLDFeatureStructureEditor editor,
			BlockPanel blockPanel)
	{
		this.editor = editor;
		this.blockPanel = blockPanel;
		this.sig = editor.sig;
	}
	
	@Override
	public void mouseClicked(MouseEvent e) 
	{
		//react to left click by offering a menu of options for type manipulation
		//TODO: might be too obnoxious, perhaps change this into a double click
		if (blockPanel.getSelectedBlock() != null)
		{
			Block selectedBlock = blockPanel.getSelectedBlock();
			editor.processContextStructure(selectedBlock);

			//generate context menu for structure manipulation
			TraleSLDFeatureStructureEditorMenu menu = editor.createAppropriateContextMenu();
			//in case of no signature or unknown type, the menu will be null
			if (menu != null)
			{
				menu.show(e.getComponent(),e.getX(), e.getY());
			}
		}
	}

	@Override
	public void mouseEntered(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mouseExited(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mousePressed(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mouseReleased(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}

}
