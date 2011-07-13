package org.kahina.core.util;

import java.util.HashMap;
import java.util.Map;

/**
 * Class to avoid creating many equal objects.
 * @author ke
 *
 */
public class Sharer<T>
{
	
	private final Map<T, T> objects = new HashMap<T, T>();
	
	public T share(T object)
	{
		T result = objects.get(object);
		if (result == null)
		{
			objects.put(object, object);
			return object;
		}
		return result;
	}

}
