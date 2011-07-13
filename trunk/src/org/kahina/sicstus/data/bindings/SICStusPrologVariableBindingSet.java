package org.kahina.sicstus.data.bindings;

import java.util.Map;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

import org.kahina.core.data.KahinaObject;

public class SICStusPrologVariableBindingSet extends KahinaObject
{
	
	private static final long serialVersionUID = -4543111507445440926L;
	
	private SortedSet<String> variableNames;
	
	private Map<String, String> inValues;
	
	private Map<String, String> outValues;
	
	public SICStusPrologVariableBindingSet()
	{
		variableNames = new TreeSet<String>();
		inValues = new TreeMap<String, String>();
		outValues = new TreeMap<String, String>();
	}

	/**
	 * Copy constructor.
	 * @param original
	 */
	public SICStusPrologVariableBindingSet(SICStusPrologVariableBindingSet original)
	{
		variableNames = new TreeSet<String>(original.variableNames);
		inValues = new TreeMap<String, String>(original.inValues);
		outValues = new TreeMap<String, String>(original.outValues);
	}

	public void addBinding(String variableName, String direction, String value)
	{
		variableNames.add(variableName);
		
		if ("in".equals(direction))
		{
			inValues.put(variableName, value);
		} else
		{
			outValues.put(variableName, value);
		}
	}
	
	public String[] getVariableNames()
	{
		String[] result = new String[variableNames.size()];
		int i = 0;
		
		for (String variableName : variableNames)
		{
			result[i++] = variableName;
		}
		
		return result;
	}
	
	public String[] getInValues()
	{
		String[] result = new String[variableNames.size()];
		int i = 0;
		
		for (String variableName : variableNames)
		{
			result[i++] = inValues.get(variableName);
		}
		
		return result;
	}
	
	public String[] getOutValues()
	{
		String[] result = new String[variableNames.size()];
		int i = 0;
		
		for (String variableName : variableNames)
		{
			result[i++] = outValues.get(variableName);
		}
		
		return result;
	}

}
