package org.kahina.prolog;

import org.kahina.core.KahinaRunner;
import org.kahina.core.LogicProgrammingInstance;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.gui.KahinaViewRegistry;
import org.kahina.lp.profiler.LogicProgrammingProfiler;
import org.kahina.lp.visual.source.PrologJEditSourceCodeView;
import org.kahina.prolog.bridge.PrologBridge;
import org.kahina.prolog.data.bindings.PrologVariableBindingSet;
import org.kahina.prolog.gui.PrologGUI;
import org.kahina.prolog.profiler.PrologProfiler;
import org.kahina.prolog.visual.bindings.PrologVariableBindingSetView;

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
		return new PrologGUI(PrologStep.class, this, KahinaRunner.getControl());
	}

	@Override
	protected PrologState createState()
	{
		return new PrologState();
	}
	
	@Override
	protected void fillViewRegistry()
	{
		super.fillViewRegistry();
		KahinaViewRegistry.registerMapping(PrologVariableBindingSet.class, PrologVariableBindingSetView.class);
		KahinaViewRegistry.registerMapping(KahinaSourceCodeLocation.class, PrologJEditSourceCodeView.class);
	}

}
