package org.kahina.core.data.lightweight;

import java.util.List;
import java.lang.reflect.Type;

public class ListLVT extends LVT
{
	private LVT elementType;

	public static ListLVT createListLVT(Class<?> type)
	{
		Type[] arguments = typeArgumentsForInterface(type, List.class);
		if (arguments == null)
		{
			return null;
		}
		ListLVT result = new ListLVT();
		result.elementType = LVT.createLVT((Class<?>) arguments[0]);
		if (result.elementType == null)
		{
			return null;
		}
		return result;
	}

}
