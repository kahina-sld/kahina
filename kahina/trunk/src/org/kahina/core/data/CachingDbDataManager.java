package org.kahina.core.data;

import org.kahina.core.io.database.DatabaseHandler;

/**
 * A DB data manager with a very simple caching strategy, namely a cache of
 * size 1.
 * @author ke
 *
 */
public class CachingDbDataManager extends DbDataManager
{	
	private KahinaObject cachedObject;
	
	private int cachedObjectID;
	
	private static final boolean verbose = false;

	public CachingDbDataManager(DatabaseHandler db)
	{
		super(db);
	}

	@Override
	public void storeCaching(KahinaObject object)
	{
		int id = object.getID();
		if (verbose)
		{
			System.err.println(this + ".store(" + object + " (ID " + id + ")), cachedObjectID=" + cachedObjectID);
		}
		if (id != cachedObjectID)
		{
			storeCachedObject();
			cachedObject = object;
			cachedObjectID = id;
		}
		if (verbose)
		{
			System.err.println("//" + this + ".store(" + object + " (ID " + id + ")), cachedObjectID=" + cachedObjectID);
		}
	}
	
	public void storeCachedObject()
	{
		if (verbose)
		{
			System.err.println(this + ".storeCachedObject(), cachedObjectID=" + cachedObjectID);
		}
		if (cachedObject != null)
		{
			store(cachedObject);
		}
	}
	
	@Override
	public KahinaObject retrieve(int id)
	{
		if (verbose)
		{
			System.err.println(this + ".retrieve(" + id + "), cachedObjectID=" + cachedObjectID);
		}
		if (id == cachedObjectID)
		{
			return cachedObject;
		}
		return super.retrieve(id);
	}
}
