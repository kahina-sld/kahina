package org.kahina.tralesld.data.fs;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.util.ObjectUtil;

public class TraleSLDVariableBinding extends KahinaObject
{

	/**
	 * 
	 */
	private static final long serialVersionUID = -1606728351152682772L;
	public String varName;
	public String tag;
	public String type;
	public TraleSLDFS fs;

	public TraleSLDVariableBinding()
	{
	}

	public TraleSLDVariableBinding(String varName, String tag, String type, TraleSLDFS grisuMessage)
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
		return ObjectUtil.equal(varName, other.varName) && ObjectUtil.equal(tag, other.tag) && ObjectUtil.equal(type, other.type) && ObjectUtil.equal(fs, other.fs);
	}
	
	@Override
	public int hashCode()
	{
		return ObjectUtil.hashCode(varName, tag, type, fs);
	}

}
