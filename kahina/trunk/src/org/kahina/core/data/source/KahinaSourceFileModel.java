package org.kahina.core.data.source;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.lightweight.LightweightKahinaObject;
import org.kahina.core.data.text.KahinaTextWithMarking;

public class KahinaSourceFileModel extends KahinaObject implements LightweightKahinaObject
{
    public String absolutePathName;
    public List<String> lines;
    
    public KahinaSourceFileModel(String absolutePathName)
    {
        this.absolutePathName = absolutePathName;
        File sourceFile = new File(absolutePathName);
        lines = new ArrayList<String>();
        try
        {
            Scanner sourceFileScanner = new Scanner(sourceFile);
            while (sourceFileScanner.hasNextLine())
            {
                lines.add(sourceFileScanner.nextLine());
            }
        }
        catch (FileNotFoundException e)
        {
            lines.add("ERROR: could not load source file " + absolutePathName);
            System.err.println("ERROR: could not load source file " + absolutePathName);
        }       
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
