package org.kahina.tralesld.data.tree;

import org.kahina.core.data.tree.DefaultLayerDecider;
import org.kahina.core.data.tree.KahinaTree;

public class TraleSLDLayerDecider extends DefaultLayerDecider
{
	int limit;

	public TraleSLDLayerDecider()
	{
		this(2);
	}

	/**
	 * 
	 * @param limit
	 *            Limits the number of layers, higher levels will be conflated
	 *            with the limit.
	 */
	public TraleSLDLayerDecider(int limit)
	{
		this.limit = limit;
	}

	@Override
	public int decideOnLayer(int nodeID, KahinaTree tree)
	{
		if (nodeID == tree.getRootID(0))
		{
			return 0;
		}
		if (nodeID == -1)
		{
			return -1;
		}
		String nodeCaption = tree.getPrimaryModel().getNodeCaption(nodeID);
		if (nodeCaption.indexOf("rule") != -1 || nodeCaption.indexOf("lexicon") != -1)
		{
			return 0;
		}
		if (limit == 1)
		{
			return 1;
		}
		if (nodeCaption.indexOf("lex") != -1 || nodeCaption.indexOf("goal") != -1)
		{
			return 1;
		}
		return 2;
	}
}
