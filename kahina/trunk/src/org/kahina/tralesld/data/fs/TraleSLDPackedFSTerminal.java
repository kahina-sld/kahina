package org.kahina.tralesld.data.fs;

public class TraleSLDPackedFSTerminal extends TraleSLDPackedFS
{

	public String string;
	
	public TraleSLDPackedFSTerminal()
	{
	}

	public TraleSLDPackedFSTerminal(String string)
	{
		this.string = string;
	}
	
	@Override
	protected void toString(StringBuilder builder)
	{
		builder.append(string);
	}

}