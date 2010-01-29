package org.kahina.core.data;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kahina.core.KahinaException;
import org.kahina.io.database.DatabaseHandler;

public class DbDataManager extends DataManager
{
	private DatabaseHandler db;

	private Map<Class<? extends KahinaObject>, Integer> classIDByClass = new HashMap<Class<? extends KahinaObject>, Integer>();

	private List<DataStore> storeByClassID = new ArrayList<DataStore>();

	public DbDataManager(DatabaseHandler db)
	{
		this.db = db;
	}

	@Override
	protected DataStore getStoreForClass(Class<? extends KahinaObject> clazz)
	{
		return storeByClassID.get(classIDByClass.get(clazz));
	}

	@Override
	public void registerDataType(Class<? extends KahinaObject> clazz, DataStore store)
	{
		if (classIDByClass.containsKey(clazz))
		{
			throw new KahinaException("A data store for class " + clazz + " is already registered.");
		}
		classIDByClass.put(clazz, storeByClassID.size());
		storeByClassID.add(store);
	}

	@Override
	public void registerDataType(Class<? extends KahinaObject> clazz)
	{
		registerDataType(clazz, new KahinaObjectDbDataStore(clazz, this, db));
	}

	public int getClassID(Class<? extends KahinaObject> clazz)
	{
		return classIDByClass.get(clazz);
	}
}
