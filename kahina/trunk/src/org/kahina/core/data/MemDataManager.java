package org.kahina.core.data;

import java.util.HashMap;
import java.util.Map;

import org.kahina.core.KahinaException;

public class MemDataManager extends DataManager
{
	private Map<Class<? extends KahinaObject>, DataStore> storeByClass = new HashMap<Class<? extends KahinaObject>, DataStore>();

	@Override
	public void registerDataType(Class<? extends KahinaObject> clazz)
	{
		registerDataType(clazz, new KahinaObjectMemDataStore());
	}

	@Override
	public void registerDataType(Class<? extends KahinaObject> clazz, DataStore store)
	{
		if (storeByClass.containsKey(clazz))
		{
			throw new KahinaException("A data store for class " + clazz + " is already registered.");
		}
		storeByClass.put(clazz, store);
	}

	@Override
	protected DataStore getStoreForClass(Class<? extends KahinaObject> clazz)
	{
		return storeByClass.get(clazz);
	}
}
