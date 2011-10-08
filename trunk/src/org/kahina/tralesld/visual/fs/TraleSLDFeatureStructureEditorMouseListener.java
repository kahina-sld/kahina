package org.kahina.tralesld.visual.fs;

import gralej.blocks.BlockPanel;
import gralej.om.IEntity;
import gralej.om.IType;
import gralej.om.ITypedFeatureStructure;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import org.kahina.tralesld.data.signature.TraleSLDSignature;

public class TraleSLDFeatureStructureEditorMouseListener implements MouseListener
{
	BlockPanel blockPanel;
	TraleSLDSignature sig;
	
	public TraleSLDFeatureStructureEditorMouseListener(BlockPanel blockPanel, TraleSLDSignature sig)
	{
		this.blockPanel = blockPanel;
		this.sig = sig;
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
				String type = selectedType.typeName();
				System.err.println("  " + type);
				System.err.println("     Specialize: " + sig.getSubtypes(type));
				System.err.println("     Generalize: " + sig.getSupertypes(type));
			}
			else if (selectedEntity instanceof ITypedFeatureStructure)
			{
				ITypedFeatureStructure selectedFS = (ITypedFeatureStructure) selectedEntity;
				String type = selectedFS.type().typeName();
				System.err.println("  " + type);
				System.err.println("     Specialize: " + sig.getSubtypes(type));
				System.err.println("     Generalize: " + sig.getSupertypes(type));
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
