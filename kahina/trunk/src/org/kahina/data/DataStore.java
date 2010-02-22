package org.kahina.data;

/**
 * A data store is an object that stores and retrieves {@link KahinaObject}s of
 * one specific data type (i.e. runtime class). Objects are identified by their
 * IDs.
 * @author ke
 *
 */
public interface DataStore
{
	public void store(KahinaObject object);
	
	public KahinaObject retrieve(int id);
}