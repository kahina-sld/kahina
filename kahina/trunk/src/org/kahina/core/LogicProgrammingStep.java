package org.kahina.core;

import java.util.HashMap;

import org.kahina.data.source.KahinaSourceCodeLocation;

public class LogicProgrammingStep extends KahinaStep
{
    //logic programming steps possess one of five types
    private static HashMap<Integer,Integer> type = new HashMap<Integer,Integer>();
    
    //the goal description
    private static HashMap<Integer,String> goalDesc = new HashMap<Integer,String>();    
    //the step ID used by the surveyed logic programming system
    private static HashMap<Integer,Integer> externalID = new HashMap<Integer,Integer>();
    //the source code location associated with this step
    private static HashMap<Integer,Integer> codeLocation = new HashMap<Integer, Integer>();
    
    public static void copy(int copyID, int origID)
    {
        type.put(copyID, type.get(origID));
        goalDesc.put(copyID, goalDesc.get(origID));
        externalID.put(copyID, externalID.get(origID));
        codeLocation.put(copyID, codeLocation.get(origID));
    }
    
    public int getType()
    {
        return type.get(getID());
    }
    
    public static int getType(int stepID)
    {
        return type.get(stepID);
    }
    
    public void setType(int type)
    {
        LogicProgrammingStep.type.put(getID(), type);
    }
    
    public static void setType(int stepID, int type)
    {
        LogicProgrammingStep.type.put(stepID, type);
    }
    
    public String getGoalDesc()
    {
        return goalDesc.get(getID());
    }
    
    public static String getGoalDesc(int stepID)
    {
        return goalDesc.get(stepID);
    }
    
    public void setGoalDesc(String goalDesc)
    {
        LogicProgrammingStep.goalDesc.put(getID(), goalDesc);
    }
    
    public static void setGoalDesc(int stepID, String goalDesc)
    {
        LogicProgrammingStep.goalDesc.put(stepID, goalDesc);
    }
    
    public int getExternalID()
    {
        return externalID.get(getID());
    }
    
    public static int getExternalID(int stepID)
    {
        return externalID.get(stepID);
    }
    
    public void setExternalID(int externalID)
    {
        LogicProgrammingStep.externalID.put(getID(), externalID);
    }
    
    public static void setExternalID(int stepID, int externalID)
    {
        LogicProgrammingStep.externalID.put(stepID, externalID);
    }
    
    public KahinaSourceCodeLocation getSourceCodeLocation()
    {
        return KahinaSourceCodeLocation.get(codeLocation.get(getID()));
    }
    
    public static KahinaSourceCodeLocation getSourceCodeLocation(int stepID)
    {
        return KahinaSourceCodeLocation.get(codeLocation.get(stepID));
    }
    
    public void setSourceCodeLocation(String absolutePath, int lineNumber)
    {
        codeLocation.put(getID(), KahinaSourceCodeLocation.generate(absolutePath, lineNumber));
    }
    
    public static void setSourceCodeLocation(int stepID, String absolutePath, int lineNumber)
    {
        codeLocation.put(stepID, KahinaSourceCodeLocation.generate(absolutePath, lineNumber));
    }
}
