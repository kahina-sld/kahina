package org.kahina.prolog;

import org.kahina.core.LogicProgrammingInstance;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.lp.bridge.LogicProgrammingBridge;
import org.kahina.lp.gui.LogicProgrammingGUI;
import org.kahina.lp.profiler.LogicProgrammingProfiler;
import org.kahina.prolog.profiler.PrologProfiler;

public class PrologDebuggerInstance extends LogicProgrammingInstance<LogicProgrammingState, LogicProgrammingGUI, LogicProgrammingBridge>
{
	
	PrologProfiler profiler;
	
	public PrologDebuggerInstance()
	{
		profiler = new PrologProfiler(state.getFullProfile());
	}

	@Override
	public LogicProgrammingProfiler getProfiler()
	{
		return profiler;
	}

	@Override
	protected LogicProgrammingBridge createBridge()
	{
		return new LogicProgrammingBridge(state);
	}

	@Override
	protected LogicProgrammingGUI createGUI()
	{
		return new LogicProgrammingGUI(LogicProgrammingStep.class, this);
	}

	@Override
	protected LogicProgrammingState createState()
	{
		return new LogicProgrammingState();
	}

}
