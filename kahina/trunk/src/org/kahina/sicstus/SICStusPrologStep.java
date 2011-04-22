package org.kahina.sicstus;

import org.kahina.lp.LogicProgrammingStep;
import org.kahina.sicstus.data.SICStusPrologVariableBindingSet;

public class SICStusPrologStep extends LogicProgrammingStep
{

	private static final long serialVersionUID = -119683692028732745L;
	
	public SICStusPrologVariableBindingSet bindings = new SICStusPrologVariableBindingSet();
	
	public SICStusPrologStep()
	{
	}
	
	public SICStusPrologStep(SICStusPrologStep original)
	{
		super(original);
		bindings = new SICStusPrologVariableBindingSet(original.bindings);
	}
	
	@Override
	public SICStusPrologStep copy()
	{
		return new SICStusPrologStep(this);
	}

}
 