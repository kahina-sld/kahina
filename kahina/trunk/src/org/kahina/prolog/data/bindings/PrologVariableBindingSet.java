package org.kahina.prolog.data.bindings;

import org.kahina.core.data.KahinaObject;

public class PrologVariableBindingSet extends KahinaObject
{

	private static final long serialVersionUID = 9032602517905562761L;

	private String[] keys = {};

	private String[] values = {};
	
	public PrologVariableBindingSet()
	{
	}

	public PrologVariableBindingSet(PrologVariableBindingSet original)
	{
		keys = original.keys;
		values = original.values;
	}

	public void setKeys(String[] keys)
	{
		this.keys = keys;
	}

	public void setValues(String[] values)
	{
		this.values = values;
	}
	
	public String[] getKeys()
	{
		return keys;
	}
	
	public String[] getValues()
	{
		return values;
	}

}
