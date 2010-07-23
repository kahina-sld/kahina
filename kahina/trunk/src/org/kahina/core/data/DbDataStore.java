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
	protected final DbDataManager manager;
	
	protected final DatabaseHandler db;
	
	protected DbDataStore(DbDataManager manager, DatabaseHandler db)
	{
		this.manager = manager;
		this.db = db;
	}
	
}
