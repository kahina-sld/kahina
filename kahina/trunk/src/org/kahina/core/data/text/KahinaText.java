package org.kahina.core.data.text;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.lightweight.LightweightKahinaObject;

public class KahinaText extends KahinaObject implements LightweightKahinaObject, Serializable
{
    /**
	 * 
	 */
	private static final long serialVersionUID = -5142487008379123698L;
	public List<String> lines;
    
    public KahinaText()
    {
        lines = new ArrayList<String>();
    }
    
    public KahinaText(List<String> lines)
    {
    	this.lines = lines;
    }
    
    public KahinaText(String absolutePathName)
    {
        lines = new ArrayList<String>();
        File sourceFile = new File(absolutePathName);
        try
        {
            Scanner sourceFileScanner = new Scanner(sourceFile);
            while (sourceFileScanner.hasNextLine())
            {
                this.addLine(sourceFileScanner.nextLine());
            }
        }
        catch (FileNotFoundException e)
        {
            this.addLine("ERROR: could not load source file " + absolutePathName);
            System.err.println("ERROR: could not load source file " + absolutePathName);
        } 
    }
    
    public List<String> getLines()
    {
        return lines;
    }
    
    /**
     * appends a new line to the end of this text
     * @param line - the new line to be added to the text
     * @return the line ID of the new line
     */
    public int addLine(String line)
    {
        lines.add(line);
        return lines.size() - 1;
    }
    
    public String getLine(int lineID)
    {
        if (lineID < 0 || lineID >= lines.size())
        {
            return "WARNING: LINE " + lineID + " NOT DEFINED IN TEXT!";
        }
        return lines.get(lineID);
    }
    
    public static KahinaText loadFromFile(String absolutePathName)
    {
        KahinaText sourceText = new KahinaText();
        File sourceFile = new File(absolutePathName);
        try
        {
            Scanner sourceFileScanner = new Scanner(sourceFile);
            while (sourceFileScanner.hasNextLine())
            {
                sourceText.addLine(sourceFileScanner.nextLine());
            }
        }
        catch (FileNotFoundException e)
        {
            sourceText.addLine("ERROR: could not load source file " + absolutePathName);
            System.err.println("ERROR: could not load source file " + absolutePathName);
        } 
        return sourceText;
    }
    
    public String getLineContent(int lineNumber)
    {
        String result = lines.get(lineNumber - 1);
        if (result == null) result = "";
        return result;
    }
    
    public String getCompleteContent()
    {
        StringBuilder builder = new StringBuilder("");
        for (String line : lines)
        {
            builder.append(line);
        }
        return builder.toString();
    }
    
    public KahinaTextWithMarking getCompleteContentWithLineOffsets(int lineNumber)
    {
        int beginOffset = 0;
        int endOffset = 0;
        int caretIndex = 0;
        StringBuilder builder = new StringBuilder("");
        for (int i = 0; i < lines.size(); i++)
        {
            if (i == lineNumber)
            {   
                beginOffset = builder.length();
                builder.append(lines.get(i) + "\n");
                endOffset = builder.length();
                caretIndex += 1;
            }
            else if (i == lineNumber + 3)
            {
                caretIndex += builder.length() + 1;
                builder.append(lines.get(i) + "\n");
            }
            else
            {
                builder.append(lines.get(i) + "\n");
            }
        }
        return new KahinaTextWithMarking(builder.toString(),beginOffset,endOffset,caretIndex);
    }
}
