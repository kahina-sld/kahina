package org.kahina.core.data;

import org.kahina.core.io.database.DatabaseHandler;

/**
 * Abstract superclass for data stores that use a database and need to talk back
 * to their manager.
 * @author ke
 *
 */
public abstract class DbDataStore extends DataStore
{	
	protected DbDataManager manager;
	
	protected DatabaseHandler db;
	
	/**
	 * Must be called by a {@link DbDataManager} after this store is registered
	 * with it.
	 * @param manager
	 * @param db
	 */
	public void initialize(DbDataManager manager, DatabaseHandler db)
	{
		this.db = db;
		this.manager = manager;
	}
}
