package org.kahina.data.source;

import org.kahina.data.LightweightKahinaObject;

public class KahinaSourceCodeLocation extends LightweightKahinaObject
{    
    public String absolutePath;
    
    public int lineNumber;

    public KahinaSourceCodeLocation(String absolutePath, int lineNumber)
    {
        this.absolutePath = absolutePath;
        this.lineNumber = lineNumber;
    }
}
