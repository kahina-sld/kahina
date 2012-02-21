package org.kahina.core.util;

public class ObjectUtil 
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
	
	public static int nullToZero(Integer integer)
	{
		if (integer == null)
		{
			return 0;
		}
		return integer;
	}
}
