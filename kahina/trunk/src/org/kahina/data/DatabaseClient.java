package org.kahina.data;

import org.kahina.io.database.DatabaseHandler;

/**
 * {@link KahinaObject} subclasses which store their own data in the database
 * can implement this interface. A {@link DbDataManager} will then call
 * {@link #setDatabaseHandler(org.kahina.io.database.DatabaseHandler)} on the
 * object after retrieving it from the corresponding {@link DataStore}, which
 * need in this case not worry about connecting the retrieved object to the
 * database. For example, {@link LightweightKahinaObject}s can implement this
 * interface to use the database to store more information persistently than
 * the public fields {@link LightweightKahinaObjectDbDataStore} takes care of.
 * @author ke
 */
public interface DatabaseClient
{
    public void setDatabaseHandler(DatabaseHandler db);
}
