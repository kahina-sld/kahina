package org.kahina.tralesld.data.fs;

import java.util.Iterator;
import java.util.TreeSet;

import org.kahina.core.data.KahinaObject;

public class TraleSLDVariableBindingSet extends KahinaObject implements Iterable<TraleSLDVariableBinding>
{
    // declared as TreeSet, so lightweight store will construct it as such
	public TreeSet<TraleSLDVariableBinding> bindings = new TreeSet<TraleSLDVariableBinding>();

	public TraleSLDVariableBindingSet copy()
	{
		TraleSLDVariableBindingSet result = new TraleSLDVariableBindingSet();
		result.bindings.addAll(bindings);
		return result;
	}

	public void add(TraleSLDVariableBinding binding)
	{
		bindings.add(binding);
	}

	@Override
	public Iterator<TraleSLDVariableBinding> iterator()
	{
		return bindings.iterator();
	}
	
	
}
