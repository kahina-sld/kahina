package org.kahina.core.data.tree;

/**
 * Layer decider that uses layers stored in the tree model.
 * @author ke
 *
 */
public class TreeLayerDecider extends LayerDecider
{
	private static final boolean VERBOSE = false; 

	/**
	 * 
	 */
	private static final long serialVersionUID = 1124996875665227253L;

	@Override
	public int decideOnLayer(int nodeID, KahinaTree tree)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".decideOnLayer(" + nodeID + ", [tree])");
		}
		if (nodeID == -1)
		{
			return 0;
		}
		int result = tree.getLayer(nodeID);
		if (result == -1)
		{
			System.err.println("Warning: layer for node " + nodeID + "(" + tree.getNodeCaption(nodeID) + "), status: " + tree.getNodeStatus(nodeID) + ",  hasn't been set");
			return 2;
		}
		return result;
	}

}
