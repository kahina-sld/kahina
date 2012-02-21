package org.kahina.core.util;

import java.util.ArrayList;
import java.util.List;

public class MsgUtil {

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
}
