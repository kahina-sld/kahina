package org.kahina.prolog;

import org.kahina.lp.LogicProgrammingStep;
import org.kahina.prolog.data.bindings.PrologVariableBindingSet;

public class PrologStep extends LogicProgrammingStep
{

	private static final long serialVersionUID = -119683692028732745L;
	
	public PrologVariableBindingSet inBindings = new PrologVariableBindingSet();
	
	public PrologVariableBindingSet outBindings = new PrologVariableBindingSet();
	
	public PrologStep()
	{
	}
	
	public PrologStep(PrologStep original)
	{
		super(original);
		inBindings = new PrologVariableBindingSet(original.inBindings);
		outBindings = new PrologVariableBindingSet(original.outBindings);
	}
	
	@Override
	public PrologStep copy()
	{
		return new PrologStep(this);
	}

	public void setInBindings(String[] keys, String[] values)
	{
		inBindings.setKeys(keys);
		inBindings.setValues(values);
	}

	public void setOutBindings(String[] keys, String[] values)
	{
		outBindings.setKeys(keys);
		outBindings.setValues(values);
	}

}
 