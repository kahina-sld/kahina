package org.kahina.core.data;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kahina.core.KahinaException;
import org.kahina.core.io.database.DatabaseHandler;

/**
 * A {@link DataManager} implementation using a database for storage,
 * {@link LightweightKahinaObjectDbDataStore}s as default data stores for
 * {@link LightweightKahinaObject}s, and {@link KahinaObjectMemDataStore}s as
 * default data stores for other {@link KahinaObject}s.
 * 
 * There is currently no way to automatically store and retrieve data stores
 * themselves. A DB data manager that is supposed tow work with previously
 * persisted data must be set up in the same way as the data manager that
 * persisted that data. In particular, the datatypes have to be registered in
 * the same order and, of course, with functionally identical data stores.
 * 
 * @author ke
 * 
 */
public class DbDataManager extends DataManager
{

	private static final String CLIENT_ID = DataManager.class.getName();

	private static final String TABLE_NAME_PREFIX = DataManager.class
			.getSimpleName();

	private static final String OBJECT_TABLE_NAME = TABLE_NAME_PREFIX
			+ "_datatypes";

	private DatabaseHandler db;

	private PreparedStatement selectStoreForIDStatement;

	private PreparedStatement updateStoreForIDStatement;

	private PreparedStatement insertStoreForIDStatement;

	private Map<Class<? extends KahinaObject>, Integer> storeIDByType = new HashMap<Class<? extends KahinaObject>, Integer>();

	private List<DataStore> storeByStoreID = new ArrayList<DataStore>();

	public DbDataManager(DatabaseHandler db)
	{
		this.db = db;
		if (!db.isRegistered(CLIENT_ID))
		{
			createTables();
			db.register(CLIENT_ID);
		}
		prepareStatements();
	}

	private void prepareStatements()
	{
		selectStoreForIDStatement = db.prepareStatement("SELECT store_id FROM "
				+ OBJECT_TABLE_NAME + " WHERE object_id = ?");
		updateStoreForIDStatement = db.prepareStatement("UPDATE "
				+ OBJECT_TABLE_NAME + " SET store_id = ? WHERE object_id = ?");
		insertStoreForIDStatement = db.prepareStatement("INSERT INTO "
				+ OBJECT_TABLE_NAME + " (object_id, store_id) VALUES (?, ?)");
	}

	public DatabaseHandler getDatabaseHandler()
	{
		return db;
	}

	private void createTables()
	{
		db.createTable(OBJECT_TABLE_NAME, "object_id INT", "store_id INT",
				"PRIMARY KEY (object_id)");
	}

	@Override
	public void persist()
	{
		for (DataStore store : storeByStoreID)
		{
			store.persist();
		}
		// TODO persist KahinaObject.getNextID()!
	}

	@Override
	protected DataStore getStoreForType(Class<? extends KahinaObject> clazz)
	{
		return storeByStoreID.get(storeIDByType.get(clazz));
	}

	public boolean isRegistered(Class<? extends KahinaObject> type)
	{
		return storeIDByType.containsKey(type);
	}

	@Override
	public void registerDataType(Class<? extends KahinaObject> type,
			DataStore store)
	{
		if (isRegistered(type))
		{
			throw new KahinaException("A data store for type " + type
					+ " is already registered.");
		}
		storeIDByType.put(type, storeByStoreID.size());
		storeByStoreID.add(store);
		if (store instanceof DbDataStore)
		{
			((DbDataStore) store).initialize(this, db);
		}
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
			registerDataType(type, new LightweightKahinaObjectDbDataStore(type));
		} else
		{
			registerDataType(type, new KahinaObjectMemDataStore());
		}
	}

	/**
	 * Returns the internal numeric ID given to a data type by this manager.
	 * 
	 * @param type
	 * @return
	 */
	public int getTypeID(Class<? extends KahinaObject> type)
	{
		return storeIDByType.get(type);
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
		return storeByStoreID.get(typeID).retrieve(objectID);
	}

	@Override
	protected DataStore getStoreForID(int id)
	{
		try
		{
			selectStoreForIDStatement.setInt(1, id);
			return storeByStoreID.get(db
					.queryInteger(selectStoreForIDStatement));
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.");
		}
	}

	@Override
	protected void setStoreForID(int id, DataStore store)
	{
		// HACK
		throw new UnsupportedOperationException(
				"Override methods that need this method and use storeIDs instead of stores!");
	}

	protected void setStoreForID(int id, int storeID)
	{
		try
		{
			updateStoreForIDStatement.setInt(1, storeID);
			updateStoreForIDStatement.setInt(2, id);
			if (updateStoreForIDStatement.executeUpdate() == 0)
			{
				insertStoreForIDStatement.setInt(1, id);
				insertStoreForIDStatement.setInt(2, storeID);
			}
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	@Override
	public void store(KahinaObject object)
	{
		int id = object.getID();
		int storeID = storeIDByType.get(object.getClass());
		storeByStoreID.get(storeID).store(object, id);
		setStoreForID(id, storeID);
	}
}
