package org.kahina.core.data;

import org.kahina.core.KahinaRunner;

/**
 * The base class of all Kahina objects, i.e. pieces of information that can be
 * stored and retrieved using a {@link DataManager}.
 * 
 * Clients can define their own data types by extending this class. The runtime
 * class of a Kahina object is referred to as its data type.
 * 
 * Kahina objects are uniquely identified by their ID ({@link #getID()}).
 * 
 * @author ke
 * 
 */
public class KahinaObject
{

	private int id;

    private static int nextID = 1;

    /**
     * @return the ID of this object. If {@link #setID(int)} has not been called
     * before, this will set the ID of the object to the next available ID.
     */
    public final int getID()
    {
        if (id == 0)
        {
            synchronized(this)
            {
                setID();
            }
        }

        return id;
    }

    /**
     * Sets the ID of this object. Callers are responsible for ensuring that IDs
     * are unique, and that the next available ID has been set high enough using
     * {@link #setNextID}.
     * @param id
     */
    public final void setID(int id)
    {
        this.id = id;
    }

    private void setID()
    {
        id = nextID++;
    }

    public static int getNextID()
    {
        return nextID;
    }

    public static void setNextID(int nextID)
    {
        KahinaObject.nextID = nextID;
    }
    
    @Deprecated
    public void store()
    {
        KahinaRunner.store(this);
    }
    
    @Deprecated
    public void storeCaching()
    {
    	store();
    }
}
