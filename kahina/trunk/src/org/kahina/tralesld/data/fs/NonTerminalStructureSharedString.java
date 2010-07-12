package org.kahina.tralesld.data.fs;

import java.util.List;

public class NonTerminalStructureSharedString extends StructureSharedString
{
	
	private List<StructureSharedString> children;
	
	public NonTerminalStructureSharedString(List<StructureSharedString> children)
	{
		this.children = children;
	}
	
	@Override
	public void toString(StringBuilder builder)
	{
		for (StructureSharedString child : children)
		{
			child.toString(builder);
		}
	}
			

}
