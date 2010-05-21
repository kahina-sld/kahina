package org.kahina.tralesld.data.tree;

import org.kahina.core.data.tree.DefaultLayerDecider;
import org.kahina.core.data.tree.KahinaTree;

public class TraleSLDLayerDecider extends DefaultLayerDecider
{
	// TODO tripartite!
	// 0: lexicon, rule_close, rule
	// 1: lex, goal (<- when it is not under lex?)
	// 2: everything else
	
	@Override
	public int decideOnLayer(int nodeID, KahinaTree tree)
	{
		if (nodeID == tree.getRootID(0))
			return 0;
		if (nodeID == -1)
			return -1;
		String nodeCaption = tree.getPrimaryModel().getNodeCaption(nodeID);
		if (nodeCaption.indexOf("rule") != -1 || nodeCaption.indexOf("lexicon") != -1/* || primaryModel.getNodeCaption(nodeID).indexOf("goal") != -1*/)
			return 0;
		return 1;
	}
}
