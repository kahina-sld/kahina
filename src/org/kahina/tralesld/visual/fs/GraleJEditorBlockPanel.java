package org.kahina.tralesld.visual.fs;

import java.util.List;

import gralej.blocks.Block;
import gralej.blocks.BlockPanel;
import gralej.om.IRelation;
import gralej.om.IVisitable;
import gralej.om.IneqsAndResidue;

public class GraleJEditorBlockPanel extends BlockPanel
{
	public GraleJEditorBlockPanel(IVisitable model, List<IRelation> residue, List<IRelation> ineqs)
	{
		super(model,new IneqsAndResidue(ineqs,residue));
	}
	
	public Block containingBlock(int x, int y)
	{
		return BlockPanel.findContainingBlock(this.getContent(), x, y);
	}
}
