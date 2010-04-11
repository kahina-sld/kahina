package org.kahina.core.data.tree;

import org.kahina.core.data.KahinaObject;

public abstract class LayerDecider extends KahinaObject
{
	public abstract int decideOnLayer(int nodeID, KahinaTree tree);
}
