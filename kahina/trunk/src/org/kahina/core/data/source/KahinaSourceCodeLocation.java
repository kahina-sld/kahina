package org.kahina.core.data.source;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.lightweight.LightweightKahinaObject;

public class KahinaSourceCodeLocation extends KahinaObject implements LightweightKahinaObject
{    
    public String absolutePath;
    
    public int lineNumber;

    public KahinaSourceCodeLocation()
    {
        // need no-arg constructor to be lightweight
    }

    public KahinaSourceCodeLocation(String absolutePath, int lineNumber)
    {
        this.absolutePath = absolutePath;
        this.lineNumber = lineNumber;
    }
}
