package org.kahina.core.data;

/**
 * A data store is an object that stores and retrieves {@link KahinaObject}s of
 * one specific datatype (i.e. runtime class). Objects are identified by their
 * IDs.
 * @author ke
 *
 */
public abstract class DataStore
{
    public final void store(KahinaObject object)
    {
    	store(object, object.getID());
    }
    
    public abstract void store(KahinaObject object, int id);

    public abstract KahinaObject retrieve(int id);

    /**
     * {@link DataManager}s call this method when their
     * {@link DataManager#persist()} method is called to tell the store to take
     * all steps necessary to persist the objects it is responsible for.
     * @throws UnsupportedOperationException if this data store does not support
     * persistence.
     */
    public void persist()
    {
        throw new UnsupportedOperationException("This data store does not support persistence.");
    }
}
