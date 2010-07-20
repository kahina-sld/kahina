package org.kahina.tralesld;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaRunner;
import org.kahina.core.data.KahinaDataHandlingMethod;
import org.kahina.core.gui.KahinaViewRegistry;
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
		//gui = new TraleSLDGUI(TraleSLDStep.class, this);
		//bridge = new TraleSLDBridge(this, gui);
	}
	
	public TraleSLDInstance(TraleSLDState state)
	{
		super(state);
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
		KahinaViewRegistry.registerMapping(TraleSLDPackedFS.class, TraleSLDFeatureStructureView.class); // TODO not nice
		KahinaViewRegistry.registerMapping(TraleSLDVariableBindingSet.class, TraleSLDVariableBindingSetView.class);
	}
}
