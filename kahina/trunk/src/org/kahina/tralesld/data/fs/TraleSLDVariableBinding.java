package org.kahina.tralesld.data.fs;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.lightweight.LightweightKahinaObject;
import org.kahina.core.util.Utilities;

public class TraleSLDVariableBinding extends KahinaObject implements LightweightKahinaObject
{

	public String varName;
	public String tag;
	public String type;
	public TraleSLDPackedFS fs;

	public TraleSLDVariableBinding()
	{
	}

	public TraleSLDVariableBinding(String varName, String tag, String type, TraleSLDPackedFS grisuMessage)
	{
		this.varName = varName;
		this.tag = tag;
		this.type = type;
		this.fs = grisuMessage;
	}

	@Override
	public String toString()
	{
		return getClass().getName() + "(" + varName + "," + tag + "," + type + ")";
	}

	public String getVarName()
	{
		return varName;
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof TraleSLDVariableBinding))
		{
			return false;
		}
		TraleSLDVariableBinding other = (TraleSLDVariableBinding) o;
		return Utilities.equal(varName, other.varName) && Utilities.equal(tag, other.tag) && Utilities.equal(type, other.type) && Utilities.equal(fs, other.fs);
	}
	
	@Override
	public int hashCode()
	{
		return Utilities.hashCode(varName, tag, type, fs);
	}

}
