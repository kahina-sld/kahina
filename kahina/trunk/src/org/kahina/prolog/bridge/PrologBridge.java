package org.kahina.prolog.bridge;

import org.kahina.core.KahinaRunner;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.lp.LogicProgrammingStepType;
import org.kahina.lp.bridge.LogicProgrammingBridge;
import org.kahina.prolog.PrologState;
import org.kahina.prolog.PrologStep;

public class PrologBridge extends LogicProgrammingBridge
{

	public PrologBridge(PrologState state)
	{
		super(state);
	}
	
	@Override
	protected PrologStep generateStep()
	{
		return new PrologStep();
	}
	

	@Override
	public void step(int extID, String nodeLabel)
	{
		super.step(extID,nodeLabel);
		state.consoleMessage(convertStepID(extID), extID, LogicProgrammingStepType.CALL, nodeLabel);
	}
	
	/**
	 * 
	 * @param externalStepID
	 * @param direction any string, normally either {@code "in"} (for call, redo) or {@code "out"} (for exit)
	 * @param keys argument numbers or variable names
	 * @param values string representations of Prolog terms
	 */
	public void registerBindings(int externalStepID, String direction, String[] keys, String[] values)
	{
		try
		{
			int internalStepID = convertStepID(externalStepID);
			PrologStep step = KahinaRunner.retrieve(PrologStep.class, internalStepID);
			
			if ("in".equals(direction))
			{
				step.setInBindings(keys, values);
			} else
			{
				step.setOutBindings(keys, values);
			}

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
