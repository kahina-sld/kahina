package org.kahina.tralesld.data.tree;

import java.util.regex.Pattern;

import org.kahina.core.data.tree.DefaultLayerDecider;
import org.kahina.core.data.tree.KahinaTree;

public class TraleSLDLayerDecider extends DefaultLayerDecider
{
	private static final boolean verbose = false;
	
	private static final Pattern LEVEL0_PATTERN = Pattern.compile("\\d+ (rule|lexicon).*");
	
	private static final Pattern LEVEL1_PATTERN = Pattern.compile("\\d+ (cat|retrieve|goal|mother|lex).*");
	
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
		if (LEVEL0_PATTERN.matcher(nodeCaption).matches())
		{
			if (verbose)
			{
				System.err.println("Level 0 pattern matches for " + nodeCaption);
			}
			return 0;
		}
		if (limit == 1)
		{
			return 1;
		}
		if (LEVEL1_PATTERN.matcher(nodeCaption).matches())
		{
			if (verbose)
			{
				System.err.println("Level 1 pattern matches for " + nodeCaption);
			}
			return 1;
		}
		return 2;
	}
}
