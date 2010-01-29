package org.kahina.core.data;

import org.kahina.io.database.DatabaseHandler;

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
