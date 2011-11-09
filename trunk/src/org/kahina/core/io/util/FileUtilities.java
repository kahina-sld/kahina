package org.kahina.core.io.util;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

public class FileUtilities 
{
	public static String slurpFile(String file) throws IOException 
	{
	    BufferedReader reader = new BufferedReader( new FileReader (file));
	    String line  = null;
	    StringBuilder stringBuilder = new StringBuilder();
	    String ls = System.getProperty("line.separator");
	    while( ( line = reader.readLine() ) != null ) {
	        stringBuilder.append( line );
	        stringBuilder.append( ls );
	    }
	    reader.close();
	    return stringBuilder.toString();
	 }
	
	public static void writeStringToFile(String string, String file) throws IOException 
	{
	    BufferedWriter writer = new BufferedWriter( new FileWriter (file));
	    writer.append(string);
	    writer.close();
	 }
}
