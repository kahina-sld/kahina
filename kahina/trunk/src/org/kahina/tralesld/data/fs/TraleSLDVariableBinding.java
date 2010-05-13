package org.kahina.tralesld.data.fs;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.lightweight.LightweightKahinaObject;

public class TraleSLDVariableBinding extends KahinaObject implements LightweightKahinaObject
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
	
	@Override
	public String toString()
	{
		return getClass().getName() + "(" + varName + "," + tag + "," + type + ")";
	}

	public String getVarName()
	{
		return varName;
	}

}
