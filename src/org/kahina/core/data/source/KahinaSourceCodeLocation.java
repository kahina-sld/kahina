package org.kahina.core.data.source;

import java.util.HashMap;
import java.util.Map;

import org.kahina.core.data.KahinaObject;

public class KahinaSourceCodeLocation extends KahinaObject
{    

	private static final long serialVersionUID = 2916702640971956581L;

	private static final Map<String, String> ABSOLUTE_PATHS = new HashMap<String, String>();
	
	private String absolutePath;
	
	private int lineNumber;
    
    public KahinaSourceCodeLocation(String absolutePath, int lineNumber)
    {
    	// use old String object if equal
    	String oldAbsolutePath = ABSOLUTE_PATHS.get(absolutePath);
    	if (oldAbsolutePath == null)
    	{
    		ABSOLUTE_PATHS.put(absolutePath, absolutePath);
    		oldAbsolutePath = absolutePath;
    	}
    	this.absolutePath = oldAbsolutePath;
    	this.lineNumber = lineNumber;
    }
    
    public String getAbsolutePath()
    {
    	return absolutePath;
    }
    
    public int getLineNumber()
    {
    	return lineNumber;
    }
    
}
