package org.kahina.tralesld.data.tree;

import org.kahina.core.data.tree.DefaultLayerDecider;
import org.kahina.core.data.tree.KahinaTree;

public class TraleSLDLayerDecider extends DefaultLayerDecider
{
	public int decideOnLayer(int nodeID, KahinaTree tree)
	{
		if (nodeID == tree.getRootID(0))
			return 0;
		if (nodeID == -1)
			return -1;
		KahinaTree primaryModel = tree.getPrimaryModel();
		if (primaryModel.getNodeCaption(nodeID).indexOf("rule") != -1 || primaryModel.getNodeCaption(nodeID).indexOf("\"") != -1)
			return 0;
		return 1;
	}
}
