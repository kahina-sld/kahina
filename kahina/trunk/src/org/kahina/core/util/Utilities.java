package org.kahina.core.util;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;


public class Utilities
{
	public static boolean equal(Object one, Object another)
	{
		if (one == null)
		{
			return another == null;
		}
		return one.equals(another);
	}

	public static int hashCode(Object... objects)
	{
		int result = 31 + hashCode(objects[0]);

		for (int i = 1; i < objects.length; i++)
		{
			result *= 31 + hashCode(objects[i]);
		}
		return result;
	}

	public static int hashCode(Object object)
	{
		if (object == null)
		{
			return 0;
		}

		return object.hashCode();
	}

	public static List<String> portrayStackTrace(Throwable t)
	{
		List<String> result = new ArrayList<String>();
		result.add(t.toString());
		for (StackTraceElement element : t.getStackTrace())
		{
			result.add("        at " + element);
		}
		t = t.getCause();
		while (t != null)
		{
			result.add("Caused by: " + t);
			for (StackTraceElement element : t.getStackTrace())
			{
				result.add("        at " + element);
			}
			t = t.getCause();
		}
		return result;
	}
	
	public static String join(String glue, Iterable<?> pieces)
	{
		StringBuilder result = new StringBuilder();
		Iterator<?> it = pieces.iterator();
		if (it.hasNext())
		{
			result.append(it.next());
		}
		while (it.hasNext())
		{
			result.append(glue);
			result.append(it.next());
		}
		return result.toString();
	}
}
