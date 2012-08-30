package org.kahina.lp;

import org.kahina.core.KahinaInstance;
import org.kahina.lp.behavior.LogicProgrammingTreeBehavior;
import org.kahina.lp.bridge.LogicProgrammingBridge;
import org.kahina.lp.gui.LogicProgrammingGUI;
import org.kahina.lp.profiler.LogicProgrammingProfiler;

public abstract class LogicProgrammingInstance<S extends LogicProgrammingState, G extends LogicProgrammingGUI, B extends LogicProgrammingBridge> extends KahinaInstance<S, G, B>
{

	public abstract LogicProgrammingProfiler getProfiler();
    
    public S getState()
    {
        return state;
    }
	
	@Override
	protected void createTreeBehavior()
	{
		new LogicProgrammingTreeBehavior(state.getStepTree(), this, state.getSecondaryStepTree());	
	}
}
