package org.kahina.tralesld.data.fs;



public class TraleSLDPackedFSTerminal extends TraleSLDPackedFS
{

	private String string;

	public TraleSLDPackedFSTerminal(String string)
	{
		this.string = string;
	}
	
	@Override
	public void toString(StringBuilder builder)
	{
		builder.append(string);
	}

}