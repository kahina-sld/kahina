package org.kahina.data.source;

import org.kahina.core.data.KahinaObject;

public class KahinaSourceCodeLocation extends KahinaObject
{
    static int lastID = 0;
    static String type = "KahinaSourceCodeLocation";
    
    public KahinaSourceCodeLocation()
    {
        super(lastID++);
    }
}
