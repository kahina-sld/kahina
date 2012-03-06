package org.kahina.lp;

import org.kahina.core.KahinaRunner;
import org.kahina.core.KahinaStep;
import org.kahina.core.data.source.KahinaSourceCodeLocation;

public class LogicProgrammingStep extends KahinaStep
{    
    /**
	 * 
	 */
	private static final long serialVersionUID = -7522398040719763248L;
	
	private static final boolean VERBOSE = false;
	
	//the goal description
    public String goalDesc;    
    //the step ID used by the surveyed logic programming system
    public int externalID;
    //the source code location associated with this step
    public KahinaSourceCodeLocation codeLocation;
    
    public int redone;
    
    public LogicProgrammingStep()
    {
    }
    
    public LogicProgrammingStep(LogicProgrammingStep original)
	{
    	goalDesc = original.goalDesc;
    	externalID = original.externalID;
    	codeLocation = original.codeLocation;
    	redone = original.redone;
	}

	public LogicProgrammingStep copy()
    {
    	return new LogicProgrammingStep(this);
    }
    
    public String getGoalDesc()
    {
        return goalDesc;
    }
    
    public void setGoalDesc(String goalDesc)
    {
    	if (VERBOSE)
    	{
    		System.err.println(this + ".setGoalDesc(" + goalDesc + ")");
    	}
        this.goalDesc = goalDesc;
    }
    
    public int getExternalID()
    {
        return externalID;
    }
    
    public void setExternalID(int externalID)
    {
        this.externalID = externalID;
    }
    
    public KahinaSourceCodeLocation getSourceCodeLocation()
    {
        return codeLocation;
    }
    
    public void setSourceCodeLocation(KahinaSourceCodeLocation codeLocation)
    {
        this.codeLocation = codeLocation;
    }
    
    public int getRedone()
    {
    	return redone;
    }
    
    public void incrementRedone()
    {
    	redone++;
    }
    
    public boolean isRedone()
    {
    	return redone > 0;
    }
    
    @Override
    public String toString()
    {
    	String result = "step " + externalID;
    	if (redone != 0)
    	{
    		result += " (redo " + redone + ")";
    	}
    	return result;
    }
}
