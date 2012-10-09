package org.kahina.core.control;

import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class KahinaCodeLineProperty extends KahinaStepProperty
{
    KahinaSourceCodeLocation location;
    
    public KahinaCodeLineProperty(KahinaSourceCodeLocation location)
    {
        this.location = location;
    }
    
    @Override
    public KahinaCodeLineProperty copy()
    {
        return new KahinaCodeLineProperty(location.copy());
    }

    @Override
    public Element exportXML(Document dom)
    {
        // TODO Auto-generated method stub
        return null;
    }

    public static KahinaCodeLineProperty importXML(Element el)
    {
        // TODO Auto-generated method stub
        return null;
    }

    public boolean matches(KahinaSourceCodeLocation otherLocation)
    {
        if (otherLocation == null) return false;
        System.err.println(location.getAbsolutePath() + " vs " + otherLocation.getAbsolutePath());
        System.err.println(location.getLineNumber() + " vs " + otherLocation.getLineNumber());
        //exploit the fact that KahinaSourceCodeLocation enforces identity of equal absolute paths
        if (location.getAbsolutePath() != otherLocation.getAbsolutePath()) return false;
        if (location.getLineNumber() != otherLocation.getLineNumber()) return false;
        return true;
    }

    public KahinaSourceCodeLocation getCodeLocation()
    {
        return location;
    }
}
