package org.kahina.data.source;

import org.kahina.data.KahinaObject;
import org.kahina.data.LightweightKahinaObject;

public class KahinaSourceCodeLocation extends LightweightKahinaObject
{
    static int lastID = 0;
    static String type = "KahinaSourceCodeLocation";
    
    public String absolutePath;
    public int lineNumber;
    
    public KahinaSourceCodeLocation()
    {
        super(lastID++);
    }
    
    public static KahinaSourceCodeLocation get(int id)
    {
        return (KahinaSourceCodeLocation) mng.retrieve(KahinaSourceCodeLocation.class, id);
    }
    
    public static int generate(String absolutePath, int lineNumber)
    {
        KahinaSourceCodeLocation newCodeLoc = new KahinaSourceCodeLocation();
        newCodeLoc.absolutePath = absolutePath;
        newCodeLoc.lineNumber = lineNumber;
        mng.store(newCodeLoc);
        return newCodeLoc.getID();
    }
}
