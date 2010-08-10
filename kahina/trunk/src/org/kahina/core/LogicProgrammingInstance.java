package org.kahina.core;

import org.kahina.lp.LogicProgrammingState;
import org.kahina.lp.bridge.LogicProgrammingBridge;
import org.kahina.lp.gui.LogicProgrammingGUI;
import org.kahina.lp.profiler.LogicProgrammingProfiler;

public abstract class LogicProgrammingInstance<S extends LogicProgrammingState, G extends LogicProgrammingGUI, B extends LogicProgrammingBridge> extends KahinaInstance<S, G, B>
{
	
	public LogicProgrammingInstance()
	{
		super();
	}
	
	public LogicProgrammingInstance(S state)
	{
		super(state);
	}

	public abstract LogicProgrammingProfiler getProfiler();
}
