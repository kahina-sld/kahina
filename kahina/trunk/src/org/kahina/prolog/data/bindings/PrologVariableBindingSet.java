package org.kahina.prolog.data.bindings;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

public class PrologVariableBindingSet implements Serializable
{

	private static final long serialVersionUID = 9032602517905562761L;

	private Map<String, String[]> keysByDirection = null;

	private Map<String, String[]> valuesByDirection = null;
	
	public PrologVariableBindingSet()
	{
	}

	public PrologVariableBindingSet(PrologVariableBindingSet original)
	{
		keysByDirection = new HashMap<String, String[]>(original.keysByDirection);
		valuesByDirection = new HashMap<String, String[]>(original.valuesByDirection);
	}

	public void setKeys(String direction, String[] keys)
	{
		if (keysByDirection == null)
		{
			keysByDirection = new HashMap<String, String[]>();
		}

		keysByDirection.put(direction, keys);
	}

	public void setValues(String direction, String[] values)
	{
		if (valuesByDirection == null)
		{
			valuesByDirection = new HashMap<String, String[]>();
		}

		valuesByDirection.put(direction, values);
	}
	
	public String[] getKeys(String direction)
	{
		return keysByDirection.get(direction);
	}
	
	public String[] getValues(String direction)
	{
		return valuesByDirection.get(direction);
	}

}
