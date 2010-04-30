package org.kahina.core.data.text;

import java.util.ArrayList;
import java.util.List;

import org.kahina.core.data.KahinaObject;

public class KahinaText extends KahinaObject
{
    List<String> lines;
    
    public KahinaText()
    {
        lines = new ArrayList<String>();
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
        return lines.get(lineID);
    }
}
