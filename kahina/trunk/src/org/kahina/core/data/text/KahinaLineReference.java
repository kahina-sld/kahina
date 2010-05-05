package org.kahina.core.data.text;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.lightweight.LightweightKahinaObject;

public class KahinaLineReference extends KahinaObject implements LightweightKahinaObject
{
    public KahinaText text;
    public int line;
    
    public int step;
    
    public KahinaLineReference()
    {
        this.text = null;
        this.line = -1;
        this.step = -1;
    }
    
    public KahinaLineReference(int line, int step)
    {
        this.text = null;
        this.line = line;
        this.step = step;
    }
    
    public KahinaLineReference(KahinaText text, int line, int step)
    {
        this.text = text;
        this.line = line;
        this.step = step;
    }
    
    public String toString()
    {
        return text.getLine(line);
    }
    
    public int getStepID()
    {
        return step;
    }
    
    public KahinaText getText()
    {
        return text;
    }
    
    public int getLine()
    {
        return line;
    }
}
