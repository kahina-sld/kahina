package org.kahina.tralesld.data.fs;

import java.io.Serializable;
import java.util.List;


public class TraleSLDPackedFSNonTerminal extends TraleSLDPackedFS implements Serializable
{
	
	/**
	 * 
	 */
	private static final long serialVersionUID = -4488083322946006628L;
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
