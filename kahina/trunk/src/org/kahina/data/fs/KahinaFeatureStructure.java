package org.kahina.data.fs;

import org.kahina.core.data.KahinaObject;

public class KahinaFeatureStructure extends KahinaObject
{
    static int lastID = 0;
    static String type = "KahinaFeatureStructure";
    
    public KahinaFeatureStructure()
    {
        super(lastID++);
    }
}
