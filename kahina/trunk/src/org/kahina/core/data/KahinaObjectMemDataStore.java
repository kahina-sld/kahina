package org.kahina.core.data;

import java.util.HashMap;
import java.util.Map;



public class KahinaObjectMemDataStore implements DataStore
{
	private int nextFreeID = 0;
	
	private Map<Integer, KahinaObject> objectByID = new HashMap<Integer, KahinaObject>();

	@Override
	public KahinaObject retrieve(int objectID)
	{
		return objectByID.get(objectID);
	}

	@Override
	public void store(KahinaObject object, int objectID)
	{
		objectByID.put(objectID, object);
		nextFreeID = Math.max(objectID, nextFreeID);
	}

	@Override
	public int store(KahinaObject object)
	{
		int objectID = nextFreeID++;
		objectByID.put(objectID, object);
		return objectID;
	}
}
