package org.kahina.tralesld.data.fs;

import java.util.List;


public class TraleSLDPackedFSNonTerminal extends TraleSLDPackedFS
{
	
	private List<TraleSLDPackedFS> children;
	
	public TraleSLDPackedFSNonTerminal(List<TraleSLDPackedFS> children)
	{
		this.children = children;
	}
	
	@Override
	public void toString(StringBuilder builder)
	{
		for (TraleSLDPackedFS child : children)
		{
			child.toString(builder);
		}
	}
			

}
