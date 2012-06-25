package org.kahina.prolog.bridge;

import org.kahina.lp.LogicProgrammingInstance;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.lp.bridge.LogicProgrammingBridge;

public class PrologBridge extends LogicProgrammingBridge
{
	
	private static final boolean VERBOSE = false;
	
	Integer queryRootID = null;

	public PrologBridge(LogicProgrammingInstance kahina)
	{
		super(kahina);
	}
	
	/**
	 * The Prolog tracers have a virtual root, allowing for Redoing the query
	 * root. Thus, returns true if the given step ID is a <em>child</em> of the
	 * tree root.
	 */
	@Override
	protected boolean isQueryRoot(int stepID)
	{
		boolean result = super.isQueryRoot(state.getStepTree().getParent(stepID));
		if (VERBOSE)
		{
			System.err.println(this + ".isQueryRoot(" + stepID + ") = " + result);
		}
		return result;
	}

}
