package org.kahina.core.bridge;

import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;


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
