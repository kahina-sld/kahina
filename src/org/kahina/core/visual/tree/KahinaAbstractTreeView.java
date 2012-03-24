package org.kahina.core.visual.tree;

import java.awt.Color;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.visual.KahinaView;

public abstract class KahinaAbstractTreeView extends KahinaView<KahinaTree>
{

	public KahinaAbstractTreeView(KahinaInstance<?, ?, ?> kahina)
	{
		super(kahina);
	}
	
	public abstract void displaySecondaryTree(KahinaTree treeModel);
	
	public abstract void setStatusColorEncoding(int status, Color color);
	
	public abstract KahinaTree getSecondaryModel();

}