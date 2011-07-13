package org.kahina.core.event;


public class KahinaStepDescriptionEvent extends KahinaEvent
{
	
	private final int stepID;
	
	private final String description;

	public KahinaStepDescriptionEvent(int stepID, String description)
	{
		super(KahinaEventTypes.STEP_DESCRIPTION);
		this.stepID = stepID;
		this.description = description;
	}
	
	public int getStepID()
	{
		return stepID;
	}
	
	public String getDescription()
	{
		return description;
	}

}
