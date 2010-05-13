package org.kahina.lp.data.text;

import org.kahina.core.data.text.KahinaLineReference;
import org.kahina.core.data.text.KahinaText;
import org.kahina.lp.LogicProgrammingStepType;

public class LogicProgrammingLineReference extends KahinaLineReference
{
    //this takes constant values from LogicProgrammingStepType
    public int port;
    
    //store the externalID for synthesis of the console line
    public int extID;
    
    public LogicProgrammingLineReference()
    {
    	super();
    }
    
    public LogicProgrammingLineReference(KahinaText consoleMessages, int lineID, int stepID, int extID, int port)
    {
        super(consoleMessages,lineID,stepID);
        this.extID = extID;
        this.port = port;
    }
    
    public LogicProgrammingLineReference generatePortVariant(int newPort)
    {
        return new LogicProgrammingLineReference(text, line, step, extID, newPort);
    }
    
    public String toString()
    {
        String portString = "";
        switch (port)
        {
            case LogicProgrammingStepType.FAIL:
            {
                portString = " Fail: ";
                break;
            }
            case LogicProgrammingStepType.CALL:
            {
                portString = " Call: ";
                break;
            }
            case LogicProgrammingStepType.EXIT:
            {
                portString = " Exit: ";
                break;
            }
            case LogicProgrammingStepType.DET_EXIT:
            {
                portString = " DetExit: ";
                break;
            }
            case LogicProgrammingStepType.REDO:
            {
                portString = " Redo: ";
                break;
            }
        }
        return extID + portString + text.getLine(line);
    }
}
