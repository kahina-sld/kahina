package org.kahina.prolog.bridge;

import org.kahina.lp.LogicProgrammingState;
import org.kahina.lp.bridge.LogicProgrammingBridge;

public class PrologBridge extends LogicProgrammingBridge
{
	
	Integer queryRootID = null;

	public PrologBridge(LogicProgrammingState state)
	{
		super(state);
	}
	
	/**
	 * The Prolog tracers have a virtual root, allowing for Redoing the query
	 * root. Thus, returns true if the given step ID is a <em>child</em> of the
	 * tree root.
	 */
	@Override
	protected boolean isQueryRoot(int stepID)
	{
		return super.isQueryRoot(state.getStepTree().getParent(stepID));
	}

}
