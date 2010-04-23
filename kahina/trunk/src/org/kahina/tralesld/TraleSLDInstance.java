package org.kahina.tralesld;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaRunner;
import org.kahina.core.data.KahinaDataHandlingMethod;
import org.kahina.core.gui.KahinaViewRegistry;
import org.kahina.tralesld.behavior.TraleSLDTreeBehavior;
import org.kahina.tralesld.bridge.TraleSLDBridge;
import org.kahina.tralesld.data.fs.TraleSLDFeatureStructure;
import org.kahina.tralesld.gui.TraleSLDGUI;
import org.kahina.tralesld.visual.fs.TraleSLDFeatureStructureView;

public class TraleSLDInstance extends KahinaInstance
{
	TraleSLDState state;
	TraleSLDBridge bridge;

	public TraleSLDInstance()
	{
		if (KahinaRunner.getDatabaseHandler() != null)
		{
			state = new TraleSLDState(this, KahinaDataHandlingMethod.DATABASE);
		} else
		{
			state = new TraleSLDState(this, KahinaDataHandlingMethod.MEMORY);
		}
		// TODO: this reeks a wee bit of Bad Software Design
		new TraleSLDTreeBehavior(state.getStepTree(), controller, this, state.getSecondaryStepTree());
		gui = new TraleSLDGUI(TraleSLDStep.class, this, controller);
		bridge = new TraleSLDBridge(this, gui, controller);
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
		KahinaViewRegistry.registerMapping(TraleSLDFeatureStructure.class,
				TraleSLDFeatureStructureView.class);
	}
}
