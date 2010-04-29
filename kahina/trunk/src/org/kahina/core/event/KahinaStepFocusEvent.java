package org.kahina.core.event;

/**
 * Event to indicate that a certain step has become the center of attention,
 * either because the user clicked on it or because control flow just passed
 * through one of its ports.
 * @author ke
 *
 */
public class KahinaStepFocusEvent extends KahinaEvent
{
	
	private final int stepID;

	public KahinaStepFocusEvent(int stepID)
	{
		super(KahinaEventTypes.STEP_FOCUS);
		this.stepID = stepID; 
	}
	
	public int getStepID()
	{
		return stepID;
	}

}
