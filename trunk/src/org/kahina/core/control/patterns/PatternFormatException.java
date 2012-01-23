package org.kahina.core.control.patterns;


/**
 * An exception type thrown when validating the values of a {@link TreeNodePattern}.
 * <p>
 * This type of exception is caught by the breakpoint editor in order to provide hints to the user.
 * @author jd
 *
 */
public class PatternFormatException extends Exception
{
    private String desiredType;
    private String badString;
    
    /**
     * Class constructor specifying desired type and value of the offending string.
     * @param desiredType the intended type of the offending string
     * @param badString the value of the offending string
     */
    public PatternFormatException(String desiredType, String badString)
    {
        this.desiredType = desiredType;
        this.badString = badString;
    }
    
    /**
     * Returns the error message as displayed e.g. in the breakpoint editor.
     */
    @Override
	public String getMessage()
    {
        return "Invalid input: " + badString + " is not a value of type " + desiredType + ".";
    }
}
