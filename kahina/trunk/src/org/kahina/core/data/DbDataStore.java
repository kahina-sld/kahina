package org.kahina.core.data;

import org.kahina.io.database.DatabaseHandler;

/**
 * Abstract superclass for data stores that use a database and need to talk back
 * to their manager.
 * @author ke
 *
 */
public abstract class DbDataStore implements DataStore
{	
	protected DbDataManager manager;
	
	protected DatabaseHandler db;
	
	public DbDataStore(DbDataManager manager, DatabaseHandler db)
	{
		this.db = db;
		this.manager = manager;
	}
}
