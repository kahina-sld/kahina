package org.kahina.sicstus.bridge;

import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.lp.LogicProgrammingInstance;
import org.kahina.prolog.bridge.PrologBridge;
import org.kahina.sicstus.SICStusPrologStep;

public class SICStusPrologBridge extends PrologBridge
{

	public SICStusPrologBridge(LogicProgrammingInstance<?,?,?> kahina)
	{
		super(kahina);
	}

	@Override
	protected SICStusPrologStep generateStep()
	{
		return new SICStusPrologStep();
	}

	public void registerBinding(int externalStepID, String direction, String variableName, String value)
	{
		try
		{
			int internalStepID = convertStepID(externalStepID);
			SICStusPrologStep step = state.retrieve(SICStusPrologStep.class, internalStepID);
			step.bindings.addBinding(variableName, direction, value);
			state.store(internalStepID, step);

			if (getBridgeState() == 'n')
			{
				kahina.dispatchEvent(new KahinaSelectionEvent(internalStepID));
			}
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

}
