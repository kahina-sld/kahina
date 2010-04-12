package org.kahina.core.data;

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

    private static final String TABLE_NAME_PREFIX = DataManager.class.getSimpleName();

    private static final String DATATYPE_TABLE_NAME = TABLE_NAME_PREFIX + "_datatypes";

    private DatabaseHandler db;

    private Map<Class<? extends KahinaObject>, Integer> typeIDByType = new HashMap<Class<? extends KahinaObject>, Integer>();

    private List<DataStore> storeByTypeID = new ArrayList<DataStore>();

    public DbDataManager(DatabaseHandler db)
    {
        this.db = db;
        if (!db.isRegistered(CLIENT_ID))
        {
            createTables();
            db.register(CLIENT_ID);
        }
    }

    // TODO is this method necessary?
    public DatabaseHandler getDatabaseHandler()
    {
        return db;
    }

    private void createTables()
    {
        db.execute("CREATE TABLE " + DATATYPE_TABLE_NAME + " (type VARCHAR(255) PRIMARY KEY, next_id INT)");
    }

    @Override
    public void persist()
    {
        for (DataStore store : storeByTypeID)
        {
            store.persist();
        }
        // TODO persist KahinaObject.getNextID()!
    }

    @Override
    protected DataStore getStoreForType(Class<? extends KahinaObject> clazz)
    {
        return storeByTypeID.get(typeIDByType.get(clazz));
    }

    public boolean isRegistered(Class<? extends KahinaObject> type)
    {
        return typeIDByType.containsKey(type);
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
        typeIDByType.put(type, storeByTypeID.size());
        storeByTypeID.add(store);
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

	@Override
    public <T extends KahinaObject> T retrieve(Class<T> type, int id)
    {
        return (T) super.retrieve(type, id);
    }
}
