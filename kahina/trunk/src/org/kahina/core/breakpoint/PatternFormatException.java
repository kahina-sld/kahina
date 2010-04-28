package org.kahina.core.breakpoint;

public class PatternFormatException extends Exception
{
	private static final long serialVersionUID = -7747363956775016819L;
	
	String desiredType;
    String badString;
    
    public PatternFormatException(String desiredType, String badString)
    {
        this.desiredType = desiredType;
        this.badString = badString;
    }
    
    public String getMessage()
    {
        return "Invalid input: " + badString + " is not a value of type " + desiredType + ".";
    }
}
