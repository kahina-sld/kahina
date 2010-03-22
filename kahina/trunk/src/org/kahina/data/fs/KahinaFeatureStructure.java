package org.kahina.data.fs;

import org.kahina.data.LightweightKahinaObject;

public class KahinaFeatureStructure implements LightweightKahinaObject
{
    private static int lastID = 0;
    
    private final int id;
    
    public KahinaFeatureStructure()
    {
    	this(lastID++);
    }
    
    public KahinaFeatureStructure(int id)
    {
    	this.id = id;
    }

	@Override
	public int getID()
	{
		return id;
	}
}
