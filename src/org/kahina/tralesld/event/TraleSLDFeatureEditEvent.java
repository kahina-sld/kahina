package org.kahina.tralesld.event;

import org.kahina.core.event.KahinaEvent;

public class TraleSLDFeatureEditEvent extends KahinaEvent 
{
	String editMessage;
	int msgType;
	
	//values for msgType
	public static final int INFO_MESSAGE = 0;
	public static final int SUCCESS_MESSAGE = 1;
	public static final int FAILURE_MESSAGE = 2;
	public static final int WARNING_MESSAGE = 3;
	public static final int SUCCESS = 4;
	//synchronizes the buffered structures of workbench and editor with common controller
	public static final int COPY_FS = 5;
	//signals to the workbench that the editor has produced a new GRISU string
	public static final int UPDATE_FS = 6;
	    
	public TraleSLDFeatureEditEvent(String editMessage, int msgType)
	{
		super(TraleSLDEventTypes.FS_EDITOR_MESSAGE);
		this.editMessage = editMessage;
		this.msgType = msgType;
	}
   
	public String getEditMessage()
	{
		return editMessage;
	}
	
	public int getMessageType()
	{
		return msgType;
	}
   
	@Override
	public String toString()
   	{
       	return  "edit message: " + editMessage;
   	}
}
