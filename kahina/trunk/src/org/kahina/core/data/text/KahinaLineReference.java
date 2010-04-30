package org.kahina.core.data.text;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.lightweight.LightweightKahinaObject;

public class KahinaLineReference extends KahinaObject implements LightweightKahinaObject
{
    KahinaText text;
    int line;
    
    int step;
    
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
}
