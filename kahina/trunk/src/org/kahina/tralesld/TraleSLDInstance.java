package org.kahina.tralesld;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaRunner;
import org.kahina.core.data.KahinaDataHandlingMethod;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.gui.KahinaViewRegistry;
import org.kahina.core.gui.event.KahinaChartUpdateEvent;
import org.kahina.core.gui.event.KahinaEdgeSelectionEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.gui.event.KahinaUpdateEvent;
import org.kahina.tralesld.behavior.TraleSLDTreeBehavior;
import org.kahina.tralesld.bridge.TraleSLDBridge;
import org.kahina.tralesld.data.fs.TraleSLDPackedFS;
import org.kahina.tralesld.data.fs.TraleSLDVariableBindingSet;
import org.kahina.tralesld.gui.TraleSLDGUI;
import org.kahina.tralesld.visual.fs.TraleSLDFeatureStructureView;
import org.kahina.tralesld.visual.fs.TraleSLDVariableBindingSetView;

public class TraleSLDInstance extends KahinaInstance<TraleSLDState, TraleSLDGUI, TraleSLDBridge>
{

	public TraleSLDInstance()
	{
		// TODO: this reeks a wee bit of Bad Software Design
		new TraleSLDTreeBehavior(state.getStepTree(), this, state.getSecondaryStepTree());
		// gui = new TraleSLDGUI(TraleSLDStep.class, this);
		// bridge = new TraleSLDBridge(this, gui);
        KahinaRunner.getControl().registerListener("edge select", this);
        KahinaRunner.getControl().registerListener("update", this);
	}

	public TraleSLDInstance(TraleSLDState state)
	{
		super(state);
        KahinaRunner.getControl().registerListener("edge select", this);
        KahinaRunner.getControl().registerListener("update", this);
		// TODO create tree behavior (not persistable yet)
	}

	@Override
	protected TraleSLDBridge createBridge()
	{
		return new TraleSLDBridge(this.state);
	}

	@Override
	protected TraleSLDGUI createGUI()
	{
		return new TraleSLDGUI(TraleSLDStep.class, this);
	}

	@Override
	protected TraleSLDState createState()
	{

		if (KahinaRunner.getDatabaseHandler() != null)
		{
			return new TraleSLDState(this, KahinaDataHandlingMethod.DATABASE);
		} else
		{
			return new TraleSLDState(this, KahinaDataHandlingMethod.MEMORY);
		}
	}

	public TraleSLDState getState()
	{
		return state;
	}

	public TraleSLDBridge getBridge()
	{
		return bridge;
	}

	protected void fillViewRegistry()
	{
		super.fillViewRegistry();
		KahinaViewRegistry.registerMapping(TraleSLDPackedFS.class, TraleSLDFeatureStructureView.class);
		KahinaViewRegistry.registerMapping(TraleSLDVariableBindingSet.class, TraleSLDVariableBindingSetView.class);
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
}
