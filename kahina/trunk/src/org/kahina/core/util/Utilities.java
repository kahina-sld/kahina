package org.kahina.core.util;

public class Utilities
{
	public static boolean equal(Object one, Object another)
	{
		if (one == null) {
			return another == null;
		}
		return one.equals(another);
	}
}
