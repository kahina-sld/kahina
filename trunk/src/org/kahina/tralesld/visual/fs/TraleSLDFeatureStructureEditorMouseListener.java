package org.kahina.tralesld.visual.fs;

import gralej.blocks.BlockPanel;
import gralej.om.IEntity;
import gralej.om.IType;
import gralej.om.ITypedFeatureStructure;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

public class TraleSLDFeatureStructureEditorMouseListener implements MouseListener
{
	BlockPanel blockPanel;
	
	public TraleSLDFeatureStructureEditorMouseListener(BlockPanel blockPanel)
	{
		this.blockPanel = blockPanel;
	}
	
	@Override
	public void mouseClicked(MouseEvent arg0) 
	{
		//System.err.println("Editor clicked at position (" + arg0.getX() + "," + arg0.getY() + ")");
		if (blockPanel.getSelectedBlock() != null)
		{
			IEntity selectedEntity = blockPanel.getSelectedBlock().getModel();
			System.err.println("Selected entity: " + selectedEntity.getClass().getCanonicalName() + " " + selectedEntity.text());
			if (selectedEntity instanceof IType)
			{
				IType selectedType = (IType) selectedEntity;
				System.err.println("  is of type " + selectedType.typeName());
			}
			else if (selectedEntity instanceof ITypedFeatureStructure)
			{
				ITypedFeatureStructure selectedFS = (ITypedFeatureStructure) selectedEntity;
				System.err.println("  is a FS of type " + selectedFS.type().typeName());
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
