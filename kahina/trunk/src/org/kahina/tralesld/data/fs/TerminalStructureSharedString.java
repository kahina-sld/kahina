package org.kahina.tralesld.data.fs;


public class TerminalStructureSharedString extends StructureSharedString
{

	private String string;

	public TerminalStructureSharedString(String string)
	{
		this.string = string;
	}
	
	@Override
	public void toString(StringBuilder builder)
	{
		builder.append(string);
	}

}