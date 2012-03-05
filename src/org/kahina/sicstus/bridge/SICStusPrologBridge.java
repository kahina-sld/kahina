package org.kahina.sicstus.bridge;

import org.kahina.core.KahinaRunner;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.lp.LogicProgrammingInstance;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.prolog.bridge.PrologBridge;
import org.kahina.sicstus.SICStusPrologStep;

public class SICStusPrologBridge extends PrologBridge
{

	public SICStusPrologBridge(LogicProgrammingInstance kahina)
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
			SICStusPrologStep step = KahinaRunner.retrieve(SICStusPrologStep.class, internalStepID);
			step.bindings.addBinding(variableName, direction, value);
			KahinaRunner.store(internalStepID, step);

			if (bridgeState == 'n')
			{
				KahinaRunner.processEvent(new KahinaSelectionEvent(internalStepID));
			}
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

}
