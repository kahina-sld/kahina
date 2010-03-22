package org.kahina.data.source;

import org.kahina.data.LightweightKahinaObject;

public class KahinaSourceCodeLocation implements LightweightKahinaObject
{
    private static int lastID = 0;
    
    private final int id;
    
    public KahinaSourceCodeLocation()
    {
    	this(lastID++);
    }
    
    public KahinaSourceCodeLocation(int id)
    {
    	this.id = id;
    }

	@Override
	public int getID()
	{
		return id;
	}
}
