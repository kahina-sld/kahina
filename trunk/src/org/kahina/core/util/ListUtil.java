package org.kahina.core.util;

import java.util.Iterator;
import java.util.List;

public class ListUtil 
{
	public static int[] integerListToIntArray(List<Integer> list)
	{
		int[] result = new int[list.size()];
		for (int i = 0; i < result.length; i++)
		{
			result[i] = list.get(i);
		}
		return result;
	}
	
	public static void ensureSize(List<?> list, int minSize)
	{
		int difference = minSize - list.size();
		for (int i = difference; i > 0; i--)
		{
			list.add(null);
		}
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
	
	@SuppressWarnings("unchecked")
	public static List<String> castToStringList(Object object)
	{
		return (List<String>) object;
	}
}
