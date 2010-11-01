package org.kahina.tralesld;

import org.kahina.core.KahinaRunner;
import org.kahina.core.LogicProgrammingInstance;
import org.kahina.core.control.KahinaController;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.gui.KahinaViewRegistry;
import org.kahina.core.gui.event.KahinaChartUpdateEvent;
import org.kahina.core.gui.event.KahinaEdgeSelectionEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.core.interfaces.KahinaPrologInterface;
import org.kahina.core.interfaces.KahinaPrologInterfaceFactory;
import org.kahina.lp.profiler.LogicProgrammingProfiler;
import org.kahina.lp.visual.source.PrologJEditSourceCodeView;
import org.kahina.tralesld.behavior.TraleSLDTreeBehavior;
import org.kahina.tralesld.bridge.TraleSLDBridge;
import org.kahina.tralesld.data.fs.TraleSLDFS;
import org.kahina.tralesld.data.fs.TraleSLDVariableBindingSet;
import org.kahina.tralesld.gui.TraleSLDGUI;
import org.kahina.tralesld.profiler.TraleSLDProfiler;
import org.kahina.tralesld.visual.fs.TraleSLDFeatureStructureView;
import org.kahina.tralesld.visual.fs.TraleSLDVariableBindingSetView;

public class TraleSLDInstance extends LogicProgrammingInstance<TraleSLDState, TraleSLDGUI, TraleSLDBridge>
{
	
	private final KahinaPrologInterface prologInterface;
	
	private TraleSLDProfiler profiler;
	
	public TraleSLDInstance()
	{
		prologInterface = KahinaPrologInterfaceFactory.create();
		if (prologInterface != null)
		{
			prologInterface.executeQuery("write('Hallo Welt!'),nl.");
		} else
		{
			System.out.println("No Prolog interface. :-(");
		}
	}
	
	@Override
	public TraleSLDBridge startNewSession()
	{
		super.startNewSession();
		profiler = new TraleSLDProfiler(state.getFullProfile());
        controller.registerListener("edge select", this);
        controller.registerListener("update", this);
        return bridge;
	}
	
	@Override
	protected void createTreeBehavior()
	{
		new TraleSLDTreeBehavior(state.getStepTree(), this, state.getSecondaryStepTree());
	}

	@Override
	protected TraleSLDBridge createBridge()
	{
		return new TraleSLDBridge(this.state);
	}

	@Override
	protected TraleSLDGUI createGUI(KahinaController guiController)
	{
		return new TraleSLDGUI(TraleSLDStep.class, this, guiController);
	}

	@Override
	protected TraleSLDState createState()
	{
		return new TraleSLDState();
	}

	@Override
	protected void fillViewRegistry()
	{
		super.fillViewRegistry();
		KahinaViewRegistry.registerMapping(TraleSLDFS.class, TraleSLDFeatureStructureView.class);
		KahinaViewRegistry.registerMapping(TraleSLDVariableBindingSet.class, TraleSLDVariableBindingSetView.class);
		KahinaViewRegistry.registerMapping(KahinaSourceCodeLocation.class, PrologJEditSourceCodeView.class);
	}

	@Override
	public void processEvent(KahinaEvent e)
	{
		super.processEvent(e);
		if (e instanceof KahinaEdgeSelectionEvent)
		{
			processEdgeSelectionEvent((KahinaEdgeSelectionEvent) e);
		} else if (e instanceof KahinaUpdateEvent)
		{
			processUpdateEvent((KahinaUpdateEvent) e);
		}
	}

	private void processEdgeSelectionEvent(KahinaEdgeSelectionEvent e)
	{
		int nodeID = state.getNodeForEdge(e.getSelectedEdge());
		if (nodeID != -1)
		{
			KahinaRunner.processEvent(new KahinaSelectionEvent(nodeID));
		}
	}

	private void processUpdateEvent(KahinaUpdateEvent e)
	{
		int edgeID = state.getEdgeForNode(e.getSelectedStep());
		if (edgeID != -1)
		{
			KahinaRunner.processEvent(new KahinaChartUpdateEvent(edgeID));
		}
	}

	@Override
	public LogicProgrammingProfiler getProfiler()
	{
		return profiler;
	}
	
	public static void main(String[] args)
	{
		(new TraleSLDInstance()).start(args);
	}
	
}
