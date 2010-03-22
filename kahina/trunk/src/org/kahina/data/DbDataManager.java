package org.kahina.data;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kahina.core.KahinaException;
import org.kahina.io.database.DatabaseHandler;

/**
 * A {@link DataManager} implementation using a database for storage,
 * {@link LightweightKahinaObjectDbDataStore}s as default data stores for
 * {@link LightweightKahinaObject}s, and {@link KahinaObjectMemDataStore}s as
 * default data stores for other {@link KahinaObject}s.
 * 
 * @author ke
 * 
 */
public class DbDataManager extends DataManager
{
	private DatabaseHandler db;

	private Map<Class<? extends KahinaObject>, Integer> typeIDByType = new HashMap<Class<? extends KahinaObject>, Integer>();

	private List<DataStore> storeByTypeID = new ArrayList<DataStore>();

	public DbDataManager(DatabaseHandler db)
	{
		this.db = db;
	}

	@Override
	protected DataStore getStoreForType(Class<? extends KahinaObject> clazz)
	{
		return storeByTypeID.get(typeIDByType.get(clazz));
	}

	@Override
	public void registerDataType(Class<? extends KahinaObject> type,
			DataStore store)
	{
		if (typeIDByType.containsKey(type))
		{
			throw new KahinaException("A data store for type " + type
					+ " is already registered.");
		}
		typeIDByType.put(type, storeByTypeID.size());
		storeByTypeID.add(store);
	}

	/**
	 * Registers a new data type. If the type is a subclass of
	 * {@link LightweightKahinaObject}, objects of this type will be stored in
	 * the database, otherwise in memory.
	 */
	@Override
	public void registerDataType(Class<? extends KahinaObject> type)
	{
		if (LightweightKahinaObject.class.isAssignableFrom(type))
		{
			Class<? extends LightweightKahinaObject> castType = castToLightweight(type);
			registerDataType(type, new LightweightKahinaObjectDbDataStore(
					castType, this, db));
		} else
		{
			registerDataType(type, new KahinaObjectMemDataStore());
		}
	}

	@SuppressWarnings("unchecked")
	private Class<? extends LightweightKahinaObject> castToLightweight(
			Class<? extends KahinaObject> type)
	{
		return (Class<? extends LightweightKahinaObject>) type;
	}

	/**
	 * Returns the internal numeric ID given to a data type by this manager.
	 * 
	 * @param type
	 * @return
	 */
	public int getTypeID(Class<? extends KahinaObject> type)
	{
		return typeIDByType.get(type);
	}

	/**
	 * Retrieves an object by the internal numeric ID given to its type by this
	 * manager, and its object ID.
	 * 
	 * @param typeID
	 * @param objectID
	 * @return
	 */
	public KahinaObject retrieve(int typeID, int objectID)
	{
		return storeByTypeID.get(typeID).retrieve(objectID);
	}
}
