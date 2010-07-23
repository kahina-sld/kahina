package org.kahina.core.data.text;

import org.kahina.core.data.KahinaObject;

public class KahinaTextWithMarking extends KahinaObject
{
    public String text;
    public int beginIndex;
    public int endIndex;
    public int caretIndex;
    
    public KahinaTextWithMarking(String text, int beginIndex, int endIndex, int caretIndex)
    {
        this.text = text;
        this.beginIndex = beginIndex;
        this.endIndex = endIndex;
        this.caretIndex = caretIndex;
    }
    
    public String toString()
    {
        return "begin: " + beginIndex + " end: " + endIndex + " caret: " + caretIndex;
    }
}
