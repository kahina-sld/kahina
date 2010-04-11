package org.kahina.core.data.tree;

import org.kahina.core.data.LightweightKahinaObject;

public class DefaultLayerDecider extends LayerDecider implements LightweightKahinaObject
{
	@Override
	public int decideOnLayer(int nodeID, KahinaTree tree)
	{
		return 0;
	}
}
