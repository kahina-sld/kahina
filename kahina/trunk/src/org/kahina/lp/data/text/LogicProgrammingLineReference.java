package org.kahina.lp.data.text;

import org.kahina.core.data.text.KahinaLineReference;
import org.kahina.core.data.text.KahinaTextModel;
import org.kahina.lp.LogicProgrammingStepType;

public class LogicProgrammingLineReference extends KahinaLineReference
{

	private static final long serialVersionUID = -2368235915459798532L;

	//this takes constant values from LogicProgrammingStepType
    public int port;
    
    //store the externalID for synthesis of the console line
    public int extID;
    
    public LogicProgrammingLineReference()
    {
    	super();
    }
    
    public LogicProgrammingLineReference(KahinaTextModel consoleMessages, int lineID, int stepID, int extID, int port)
    {
        super(consoleMessages,lineID,stepID);
        this.extID = extID;
        this.port = port;
    }
    
    public LogicProgrammingLineReference generatePortVariant(int newPort)
    {
        return new LogicProgrammingLineReference(text, line, step, extID, newPort);
    }

	public LogicProgrammingLineReference generateIDVariant(int newStepID)
	{
		return new LogicProgrammingLineReference(text, line, newStepID, extID, port);
	}
    
    @Override
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
            case LogicProgrammingStepType.EXCEPTION:
            {
                portString = " Exception: ";
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
        String extIDString = "";
        if (extID != -1)
        {
            extIDString += extID;
        }
        return extIDString + portString + text.text.getLine(line);
    }
}
