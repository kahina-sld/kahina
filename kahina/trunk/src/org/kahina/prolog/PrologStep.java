package org.kahina.prolog;

import org.kahina.lp.LogicProgrammingStep;
import org.kahina.prolog.data.bindings.PrologVariableBindingSet;

public class PrologStep extends LogicProgrammingStep
{

	private static final long serialVersionUID = -119683692028732745L;
	
	public PrologVariableBindingSet bindings = new PrologVariableBindingSet();
	
	public PrologStep()
	{
	}
	
	public PrologStep(PrologStep original)
	{
		super(original);
		bindings = new PrologVariableBindingSet(original.bindings);
	}
	
	@Override
	public PrologStep copy()
	{
		return new PrologStep(this);
	}

	public void setBindings(String direction, String[] keys, String[] values)
	{
		bindings.setKeys(direction, keys);
		bindings.setValues(direction, values);
	}

}
 