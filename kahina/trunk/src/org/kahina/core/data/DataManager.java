package org.kahina.core.data;

import org.kahina.core.KahinaException;

/**
 * A data manager allows clients to store and retrieve pieces of data (e.g.
 * information associated with debugger steps) in the form of
 * {@link KahinaObject}s. Such an object is uniquely identified by its data type
 * (i.e. its runtime class, i.e. some subclass of {@link KahinaObject}), and its
 * ID ({@link KahinaObject.getID()}).
 * 
 * Within a data manager, each data type has its own {@link DataStore}
 * responsible for storing and retrieving instances of that data type. For each
 * data type, clients can either register their own store or let the data
 * manager create a default store. The client is responsible for ensuring that
 * for each data type that a data manager is used with, a <b>suitable</b> data
 * store is registered.
 * 
 * @author ke
 * 
 */
public abstract class DataManager
{

	// TODO Should DataManager know the store for each ID?
	// Could then choose store smarter than the user asks for it.

	/**
	 * Returns the data store that is registered for a given data type.
	 * 
	 * @param type
	 * @return
	 */
	protected abstract DataStore getStoreForType(
			Class<? extends KahinaObject> type);

	protected abstract DataStore getStoreForID(int id);

	protected abstract void setStoreForID(int id, DataStore store);

	/**
	 * Registers a given data store for a given data type. Only one data store
	 * can be registered per data type. The data store should be suitable for
	 * the data type.
	 * 
	 * @param type
	 * @param store
	 */
	public abstract void registerDataType(Class<? extends KahinaObject> type,
			DataStore store);

	/**
	 * Registers a data type and let the data manager pick a data store. Clients
	 * should make sure the data manager can pick a suitable store for the data
	 * type.
	 * 
	 * @param type
	 */
	public abstract void registerDataType(Class<? extends KahinaObject> type);

	/**
	 * Stores an object. The data type (i.e. runtime class) of the object must
	 * have been registered with this data manager before.
	 * 
	 * @param object
	 */
	public void store(KahinaObject object)
	{
		int id = object.getID();
		Class<? extends KahinaObject> clazz = object.getClass();
		DataStore store = getStoreForType(clazz);
		if (store == null)
		{
			throw new KahinaException("Attempted to store object with unregistered type " + clazz);
		}
		//System.err.println("storing object " + id + " of type " + object.getClass() + " in " + store);
		setStoreForID(id, store);
		store.store(object);
	}
	
	/**
	 * Behaves like {@link #store(KahinaObject)}, but if this data manager
	 * implements a caching strategy, then this object is not directly written
	 * to the backend storage (such as a database), but cached instead. Use this
	 * if this object is likely to be retrieved soon.
	 * @param object
	 */
	public void storeCaching(KahinaObject object)
	{
		// no caching strategy implemented
		store(object);
	}

	/**
	 * Retrieves a stored object by its data type and ID.
	 * 
	 * @param type
	 *            - the type of the desired object (static class field of a
	 *            class extending KahinaObject)
	 * @param id
	 *            - the type-specific ID of the desired object
	 * @return the desired object as a KahinaObject which can be cast to the
	 *         original type
	 */
	@SuppressWarnings("unchecked")
	public final <T extends KahinaObject> T retrieve(Class<T> type, int id)
	{
		return (T) retrieve(id);
	}

	public KahinaObject retrieve(int id)
	{
		DataStore store = getStoreForID(id);
		if (store == null)
		{
			throw new KahinaException("No object with ID " + id + " has been stored.");
		}
		return store.retrieve(id);
	}

	/**
	 * This method must be called to persist the stored objects across runtimes.
	 * 
	 * @throws UnsupportedOperationException
	 *             if this data manager does not support persistence.
	 */
	public void persist()
	{
		throw new UnsupportedOperationException(
				"This data manager does not support persistence.");
	}
}
