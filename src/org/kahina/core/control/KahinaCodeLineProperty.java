package org.kahina.core.control;

import org.kahina.core.data.source.KahinaSourceCodeLocation;

public class KahinaCodeLineProperty extends KahinaStepProperty
{
    KahinaSourceCodeLocation location;
    
    public KahinaCodeLineProperty(KahinaSourceCodeLocation location)
    {
        this.location = location;
    }

    public boolean matches(KahinaSourceCodeLocation otherLocation)
    {
        if (otherLocation == null) return false;
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
