package org.kahina.sicstus.bridge;

import org.kahina.core.KahinaRunner;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.lp.LogicProgrammingStepType;
import org.kahina.lp.bridge.LogicProgrammingBridge;
import org.kahina.sicstus.SICStusPrologStep;

public class SICStusPrologBridge extends LogicProgrammingBridge
{

	public SICStusPrologBridge(LogicProgrammingState state)
	{
		super(state);
	}
	
	@Override
	protected SICStusPrologStep generateStep()
	{
		return new SICStusPrologStep();
	}
	

	@Override
	public void step(int extID, String nodeLabel)
	{
		super.step(extID,nodeLabel);
		state.consoleMessage(convertStepID(extID), extID, LogicProgrammingStepType.CALL, nodeLabel);
	}
	
	public void registerBinding(int externalStepID, String direction, String variableName, String value)
	{
		int internalStepID = convertStepID(externalStepID);
		SICStusPrologStep step = KahinaRunner.retrieve(SICStusPrologStep.class, internalStepID);
		step.bindings.addBinding(variableName, direction, value);
		KahinaRunner.store(internalStepID, step);
		
		if (bridgeState == 'n')
		{
			KahinaRunner.processEvent(new KahinaSelectionEvent(internalStepID));
		}
	}

}
