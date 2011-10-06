package org.kahina.tralesld.visual.fs;

import gralej.blocks.BlockPanel;

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
		System.err.println("Selected entity: " + blockPanel.getSelectedBlock().getModel().text());
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
