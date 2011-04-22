package org.kahina.sicstus;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.gui.KahinaViewRegistry;
import org.kahina.lp.LogicProgrammingInstance;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.lp.profiler.LogicProgrammingProfiler;
import org.kahina.lp.visual.source.PrologJEditSourceCodeView;
import org.kahina.prolog.profiler.PrologProfiler;
import org.kahina.sicstus.bridge.SICStusPrologBridge;
import org.kahina.sicstus.gui.SICStusPrologGUI;

public class SICStusPrologDebuggerInstance extends LogicProgrammingInstance<LogicProgrammingState, SICStusPrologGUI, SICStusPrologBridge>
{
	
	PrologProfiler profiler;
	
	@Override
	public SICStusPrologBridge startNewSession()
	{
		super.startNewSession();
		profiler = new PrologProfiler(state.getFullProfile());
		return bridge;
	}

	@Override
	public LogicProgrammingProfiler getProfiler()
	{
		return profiler;
	}

	@Override
	protected SICStusPrologBridge createBridge()
	{
		return new SICStusPrologBridge(state);
	}

	@Override
	protected SICStusPrologGUI createGUI(KahinaController guiController)
	{
		return new SICStusPrologGUI(SICStusPrologStep.class, this, guiController);
	}

	@Override
	protected LogicProgrammingState createState()
	{
		return new LogicProgrammingState();
	}
	
	@Override
	protected void fillViewRegistry()
	{
		super.fillViewRegistry();
		//KahinaViewRegistry.registerMapping(SICStusPrologVariableBindingSet.class, SICStusPrologVariableBindingSetView.class); TODO
		KahinaViewRegistry.registerMapping(KahinaSourceCodeLocation.class, PrologJEditSourceCodeView.class);
	}
	
	public static void main(String[] args)
	{
		(new SICStusPrologDebuggerInstance()).start(args);
	}

}
