package org.kahina.breakpoint;

public class PatternFormatException extends Exception
{
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
