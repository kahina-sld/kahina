package org.kahina.tralesld.data.fs;

import java.util.Iterator;
import java.util.TreeMap;

import org.kahina.core.data.KahinaObject;

public class TraleSLDVariableBindingSet extends KahinaObject implements Iterable<TraleSLDVariableBinding>
{
	/**
	 * 
	 */
	private static final long serialVersionUID = -2345044311794474844L;

	// declared as TreeSet, so lightweight store will construct it as such
	public TreeMap<String, TraleSLDVariableBinding> bindings = new TreeMap<String, TraleSLDVariableBinding>();

	private static final boolean verbose = false;

	public TraleSLDVariableBindingSet copy()
	{
		TraleSLDVariableBindingSet result = new TraleSLDVariableBindingSet();
		result.bindings.putAll(bindings);
		return result;
	}

	public void add(TraleSLDVariableBinding binding)
	{
		if (verbose)
		{
			System.err.println("TraleSLDVariableBindingSet.add(" + binding + ")");
		}
		bindings.put(binding.getVarName(), binding);
	}

	@Override
	public Iterator<TraleSLDVariableBinding> iterator()
	{
		return bindings.values().iterator();
	}

	public int size()
	{
		if (verbose)
		{
			System.err.println("TraleSLDVariableBindingSet.size(): " + bindings.size());
		}
		return bindings.size();
	}

}
