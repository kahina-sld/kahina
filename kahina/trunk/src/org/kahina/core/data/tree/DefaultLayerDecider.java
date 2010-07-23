package org.kahina.core.data.tree;

import org.kahina.core.data.lightweight.LightweightKahinaObject;

public class DefaultLayerDecider extends LayerDecider implements LightweightKahinaObject
{
	/**
	 * 
	 */
	private static final long serialVersionUID = -4016640936973844302L;

	@Override
	public int decideOnLayer(int nodeID, KahinaTree tree)
	{
		return 0;
	}
}
