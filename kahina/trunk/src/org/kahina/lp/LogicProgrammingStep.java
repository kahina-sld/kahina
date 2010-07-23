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
	//the goal description
    public String goalDesc;    
    //the step ID used by the surveyed logic programming system
    public int externalID;
    //the source code location associated with this step
    public KahinaSourceCodeLocation codeLocation;
    
    public LogicProgrammingStep copy()
    {
    	LogicProgrammingStep copy = new LogicProgrammingStep();
    	copy.goalDesc = goalDesc;
    	copy.externalID = externalID;
    	copy.codeLocation = codeLocation;
    	return copy;
    }
    
    public String getGoalDesc()
    {
        return goalDesc;
    }
    
    public void setGoalDesc(String goalDesc)
    {
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
    
    public static LogicProgrammingStep get(int id)
    {
        return KahinaRunner.getDataManager().retrieve(LogicProgrammingStep.class, id);
    }
}
