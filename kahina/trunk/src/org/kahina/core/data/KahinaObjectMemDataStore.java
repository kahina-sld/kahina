package org.kahina.core.data;

import java.util.HashMap;
import java.util.Map;

/**
 * The default in-memory data store.
 * @author ke
 *
 */
public class KahinaObjectMemDataStore extends DataStore
{	
	private Map<Integer, KahinaObject> objectByID = new HashMap<Integer, KahinaObject>();

	@Override
	public KahinaObject retrieve(int objectID)
	{
		return objectByID.get(objectID);
	}

	@Override
	public void store(KahinaObject object)
	{
		objectByID.put(object.getID(), object);
	}
}
