package org.kahina.qtype.data.tree;

import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.data.tree.LayerDecider;

public class QTypeLayerDecider extends LayerDecider {

	/**
	 * 
	 */
	private static final long serialVersionUID = -3668167056515761575L;

	@Override
	public int decideOnLayer(int nodeID, KahinaTree tree) {
		String caption = tree.getNodeCaption(nodeID);
		if (caption.contains("compile_grammar("))
		{
			return 0;
		}
		if(caption.contains("("))
		{
			return 1;
		}
		int parentID = tree.getParent(nodeID);
		if (parentID == -1)
		{
			return 0;
		}
		return decideOnLayer(parentID, tree);
	}

}
