package org.kahina.swi;

import org.kahina.lp.LogicProgrammingStep;
import org.kahina.swi.data.bindings.SWIPrologVariableBindingSet;

public class SWIPrologStep extends LogicProgrammingStep
{

	private static final long serialVersionUID = -119683692028732745L;
	
	public SWIPrologVariableBindingSet inBindings = new SWIPrologVariableBindingSet();
	
	public SWIPrologVariableBindingSet outBindings = new SWIPrologVariableBindingSet();
	
	public SWIPrologStep()
	{
	}
	
	public SWIPrologStep(SWIPrologStep original)
	{
		super(original);
		inBindings = new SWIPrologVariableBindingSet(original.inBindings);
		outBindings = new SWIPrologVariableBindingSet(original.outBindings);
	}
	
	@Override
	public SWIPrologStep copy()
	{
		return new SWIPrologStep(this);
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
 