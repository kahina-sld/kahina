package org.kahina.prolog.bridge;

import org.kahina.lp.bridge.LogicProgrammingBridge;
import org.kahina.prolog.PrologState;
import org.kahina.prolog.PrologStep;

public class PrologBridge extends LogicProgrammingBridge
{

	public PrologBridge(PrologState state)
	{
		super(state);
	}
	
	@Override
	protected PrologStep generateStep()
	{
		return new PrologStep();
	}

}
