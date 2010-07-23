package org.kahina.core.data;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import javax.swing.ProgressMonitor;

import org.kahina.core.KahinaException;
import org.kahina.core.util.ProgressMonitorWrapper;

/**
 * A data manager using a {@link KahinaObjectMemDataStore} as a default data
 * store.
 * 
 * @author ke
 * 
 */
public class MemDataManager extends DataManager
{
	private Map<Class<? extends KahinaObject>, DataStore> storeByClass = new HashMap<Class<? extends KahinaObject>, DataStore>();

	private DataStore defaultStore = new KahinaObjectMemDataStore();

	private Map<Integer, DataStore> storeByID = new HashMap<Integer, DataStore>();

	@Override
	public void registerDataType(Class<? extends KahinaObject> clazz)
	{
		registerDataType(clazz, defaultStore);
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
	protected DataStore getStoreForType(Class<? extends KahinaObject> clazz)
	{
		return storeByClass.get(clazz);
	}

	@Override
	protected DataStore getStoreForID(int id)
	{
		return storeByID.get(id);
	}

	@Override
	protected void setStoreForID(int id, DataStore store)
	{
		storeByID.put(id, store);
	}

	@Override
	public int persistSteps()
	{
		return storeByClass.size();
	}

	@Override
	public void persist(File file, ProgressMonitorWrapper monitor)
	{
		for (DataStore store : storeByClass.values())
		{
			store.persist(getFileForStore(store, file), monitor);
			if (monitor != null)
			{
				monitor.increment();
			}
		}
		persistSelf(file);
	}

	protected void persistSelf(File file)
	{
		throw new UnsupportedOperationException("This data manager does not support persistence.");
	}

	protected File getFileForStore(DataStore store, File file)
	{
		throw new UnsupportedOperationException("This data manager does not support persistence.");
	}
	
	/**
	 * Closes all registered data stores.
	 */
	@Override
	public void close()
	{
		for (DataStore store : storeByClass.values())
		{
			store.close();
		}
	}
}
