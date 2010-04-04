package org.kahina.data;

import java.util.HashMap;
import java.util.Map;

/**
 * The base class of all Kahina objects, i.e. pieces of information that can be
 * stored and retrieved using a {@link DataManager}.
 * 
 * Clients can define their own data types by extending this class. The runtime
 * class of a Kahina object is referred to as its data type.
 * 
 * Kahina objects are uniquely identified by their data type and their ID (
 * {@link #id}, {@link #getID()}).
 * 
 * @author ke
 * 
 */
public class KahinaObject
{
    private int id;
    
    //an attempt to allow derived classes to retrieve instances of themselves
    protected static DataManager mng;

    private final static Map<Class<? extends KahinaObject>, Integer> nextIDByType = new HashMap<Class<? extends KahinaObject>, Integer>();

    /**
     * Returns the ID of this object. If {@link #setID(int)} has not been called
     * before, this will set the ID of the object to the next available ID for
     * its type.
     * @return
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

    private void setID()
    {
        synchronized (nextIDByType)
        {
            Class<? extends KahinaObject> type = getClass();

            if (nextIDByType.containsKey(type))
            {
                id = nextIDByType.get(type);
                nextIDByType.put(type, id + 1);
            } else
            {
                id = 1;
                nextIDByType.put(type, 2);
            }
        }
    }

    /**
     * Sets the ID of this object. Callers are responsible for ensuring that IDs
     * are unique for each data type, and that the next available ID for each
     * type has been set high enough using {@link #setNextID}.
     * @param id
     */
    public final void setID(int id)
    {
        this.id = id;
    }

    public static int getNextID(Class<? extends KahinaObject> type)
    {
        synchronized(nextIDByType)
        {
            return nextIDByType.get(type);
        }
    }

    public static void setNextID(Class<? extends KahinaObject> type, int id)
    {
        synchronized(nextIDByType)
        {
            nextIDByType.put(type, id);
        }
    }
    
    public static void setDataManager(DataManager mng)
    {
        KahinaObject.mng = mng;
    }
}
