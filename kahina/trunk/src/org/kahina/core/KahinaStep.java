package org.kahina.core;

import org.kahina.data.KahinaObject;

public class KahinaStep extends KahinaObject
{
    //these objects do not exist as such, but are constructed from a database via the information provider
    
    static int lastID = 0;
    static String type = "KahinaStep";
    
    public KahinaStep()
    {
        super(lastID++);
    }
}
