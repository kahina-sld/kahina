package org.kahina.tralesld.data.fs;

import java.util.List;


public class TraleSLDPackedFSNonTerminal extends TraleSLDPackedFS
{
	
	public List<TraleSLDPackedFS> children;
	
	public TraleSLDPackedFSNonTerminal()
	{	
	}
	
	public TraleSLDPackedFSNonTerminal(List<TraleSLDPackedFS> children)
	{
		this.children = children;
	}
	
	@Override
	protected void toString(StringBuilder builder)
	{
		for (TraleSLDPackedFS child : children)
		{
			child.toString(builder);
		}
	}
			

}
