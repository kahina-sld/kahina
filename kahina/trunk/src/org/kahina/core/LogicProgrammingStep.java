package org.kahina.core;

import org.kahina.data.LightweightKahinaObject;
import org.kahina.data.source.KahinaSourceCodeLocation;

public class LogicProgrammingStep extends KahinaStep implements LightweightKahinaObject
{
    //logic programming steps possess one of five types
    public int type;
    
    //the goal description
    public String goalDesc;    
    //the step ID used by the surveyed logic programming system
    public int externalID;
    //the source code location associated with this step
    public KahinaSourceCodeLocation codeLocation;
    
    public LogicProgrammingStep copy()
    {
    	LogicProgrammingStep copy = new LogicProgrammingStep();
    	copy.type = type;
    	copy.goalDesc = goalDesc;
    	copy.externalID = externalID;
    	copy.codeLocation = codeLocation;
    	return copy;
    }
    
    public int getType()
    {
        return type;
    }
    
    public void setType(int type)
    {
        this.type = type;
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
