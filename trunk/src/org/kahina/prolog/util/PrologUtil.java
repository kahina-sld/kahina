package org.kahina.prolog.util;

import java.util.LinkedList;
import java.util.List;

public class PrologUtil
{
    public static List<Integer> parsePrologIntegerList(String prologList)
    {
        List<Integer> result = new LinkedList<Integer>();
        StringBuffer charAggregator = new StringBuffer();
        char[] chars = prologList.toCharArray();
        for (int i = 1; i < chars.length - 1; i++)
        {
            if (chars[i] == ',')
            {
                result.add(Integer.parseInt(charAggregator.toString()));
                charAggregator = new StringBuffer();
            }
            else
            {
                charAggregator.append(chars[i]);
            }
        }
        if (charAggregator.length() > 0)
        {
            result.add(Integer.parseInt(charAggregator.toString()));
        }
        return result;
    }
    
    public static List<String> parsePrologStringList(String prologList)
    {
        List<String> result = new LinkedList<String>();
        StringBuffer charAggregator = new StringBuffer();
        char[] chars = prologList.toCharArray();
        for (int i = 1; i < chars.length - 1; i++)
        {
            if (chars[i] == ',')
            {
                result.add(charAggregator.toString());
                charAggregator = new StringBuffer();
            }
            else
            {
                charAggregator.append(chars[i]);
            }
        }
        if (charAggregator.length() > 0)
        {
            result.add(charAggregator.toString());
        }
        return result;
    }
    
    public static String stringToAtomLiteral(String string)
    {
    	StringBuffer result = new StringBuffer("'");
    	char[] characters = string.toCharArray();
    	
    	for (char character : characters)
    	{
    		if (character == '\'' || character == '\\')
    		{
    			result.append('\\');
    		}
    		
    		result.append(character);
    	}
    	
    	result.append("'");
    	return result.toString();
    }

	public static String atomLiteralToString(String literal)
	{
		if (literal.startsWith("'"))
		{
			return literal.substring(1, literal.length() - 1);
		}
		
		return literal;
	}
}
