package org.kahina.data.tree;

import org.kahina.data.KahinaObject;

public abstract class LayerDecider extends KahinaObject
{
	public abstract int decideOnLayer(int nodeID, KahinaTree tree);
}
