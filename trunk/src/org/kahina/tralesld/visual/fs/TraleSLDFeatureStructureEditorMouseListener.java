package org.kahina.tralesld.visual.fs;

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
		System.err.println("Editor clicked at position (" + e.getX() + "," + e.getY() + ")");
		//react to left click by offering a menu of options for type manipulation
		//TODO: might be too obnoxious, perhaps change this into a double click
		if (blockPanel.getSelectedBlock() != null)
		{
			IEntity selectedEntity = blockPanel.getSelectedBlock().getModel();
			System.err.println("Selected entity: " + selectedEntity.getClass().getCanonicalName() + " " + selectedEntity.text());
			String type = "bot";
			if (selectedEntity instanceof IType)
			{
				IType selectedType = (IType) selectedEntity;
				type = selectedType.typeName();
				System.err.println("  " + type);
				System.err.println("     Specialize: " + sig.getSubtypes(type));
				System.err.println("     Generalize: " + sig.getSupertypes(type));
			}
			else if (selectedEntity instanceof ITypedFeatureStructure)
			{
				ITypedFeatureStructure selectedFS = (ITypedFeatureStructure) selectedEntity;
				type = selectedFS.type().typeName();
				System.err.println("  " + type);
				System.err.println("     Specialize: " + sig.getSubtypes(type));
				System.err.println("     Generalize: " + sig.getSupertypes(type));
			}
			//generate context menu for type manipulation
			new TraleSLDFeatureStructureEditorMenu(editor, sig.getSubtypes(type), sig.getSupertypes(type), sig.getSiblingTypes(type)).show(e.getComponent(),e.getX(), e.getY());
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
