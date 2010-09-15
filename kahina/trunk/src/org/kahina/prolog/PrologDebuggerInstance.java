package org.kahina.prolog;

import org.kahina.core.LogicProgrammingInstance;
import org.kahina.lp.profiler.LogicProgrammingProfiler;
import org.kahina.prolog.bridge.PrologBridge;
import org.kahina.prolog.gui.PrologGUI;
import org.kahina.prolog.profiler.PrologProfiler;

public class PrologDebuggerInstance extends LogicProgrammingInstance<PrologState, PrologGUI, PrologBridge>
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
	protected PrologBridge createBridge()
	{
		return new PrologBridge(state);
	}

	@Override
	protected PrologGUI createGUI()
	{
		return new PrologGUI(PrologStep.class, this);
	}

	@Override
	protected PrologState createState()
	{
		return new PrologState();
	}

}
