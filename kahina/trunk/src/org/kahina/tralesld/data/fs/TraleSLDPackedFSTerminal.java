package org.kahina.tralesld.data.fs;

import java.io.Serializable;

public class TraleSLDPackedFSTerminal extends TraleSLDPackedFS implements Serializable
{

	/**
	 * 
	 */
	private static final long serialVersionUID = 8146983630905086694L;
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