package org.kahina.tralesld.data.fs;

import java.io.Serializable;
import java.util.List;


public class TraleSLDPackedFSNonTerminal extends TraleSLDFS implements Serializable
{
	
	/**
	 * 
	 */
	private static final long serialVersionUID = -4488083322946006628L;
	public List<TraleSLDFS> children;
	
	public TraleSLDPackedFSNonTerminal()
	{	
	}
	
	public TraleSLDPackedFSNonTerminal(List<TraleSLDFS> children)
	{
		this.children = children;
	}
	
	@Override
	protected void toString(StringBuilder builder)
	{
		for (TraleSLDFS child : children)
		{
			child.toString(builder);
		}
	}
			

}
