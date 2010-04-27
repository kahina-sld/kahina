package org.kahina.tralesld.data.fs;

import org.kahina.core.data.KahinaObject;

public class TraleSLDVariableBinding extends KahinaObject implements Comparable<TraleSLDVariableBinding>
{

	public String varName;
	public String tag;
	public String type;
	public TraleSLDFeatureStructure fs;
	
	public TraleSLDVariableBinding(String varName, String tag, String type, String grisuMessage)
	{
		this.varName = varName;
		this.tag = tag;
		this.type = type;
		this.fs = new TraleSLDFeatureStructure(grisuMessage);
	}

	/**
	 * The natural order of variable bindings is according to the variable names
	 * (alphabetically), then according to object ID.
	 */
	@Override
	public int compareTo(TraleSLDVariableBinding o)
	{
		int result = varName.compareTo(o.varName);
		if (result == 0)
		{
			return getID() - o.getID();
		}
		return result;
	}
	
	@Override
	public String toString()
	{
		return getClass().getName() + "(" + varName + "," + tag + "," + type + ")";
	}

}
